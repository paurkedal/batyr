(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Batyr_core.Prereq
open Lwt.Infix
open Lwt.Syntax
open Printf

open Api_protocol
open Common

let query_limit = 10000

let make_handler decode_request encode_response f (request : Dream.request) =
  let fail_json msg = Dream.json (Fmt.str {|{"error": \"%s\"}|} msg) in
  let* request_body = Dream.body request in
  (match request_body |> Yojson.Safe.from_string |> decode_request with
   | Ok req ->
      let* resp = f req in
      (match resp with
       | Ok resp ->
          let resp_json = resp |> encode_response |> Yojson.Safe.to_string in
          Dream.json resp_json
       | Error msg ->
          fail_json msg)
   | Error msg ->
      fail_json msg)

let sql_of_pattern pat_s =
  Batyr_core.Search.(denote_pattern (pattern_of_string pat_s))

let fetch_message_counts cond tz =
  let cond_str, Batyr_core.Search_sql.Param (params_type, params) =
    Batyr_core.Search_sql.Expr.to_sql ~first_index:2 cond
  in
  let req =
    let open Caqti_type.Std in
    let open Caqti_request.Infix in
    (tup2 string params_type ->* tup2 int int) ~oneshot:true
    (sprintf
      "SELECT \
         batyr.intenc_date(seen_time AT TIME ZONE 'UTC' AT TIME ZONE $1) AS t, \
         count(*) \
       FROM batyr.messages \
       JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
         ON sender_id = sender.resource_id \
       WHERE %s GROUP BY t"
       cond_str)
  in
  Data.Db.use begin fun (module C : Data.CONNECTION) ->
    let aux (date, count) acc = {date; count} :: acc in
    C.fold req aux (tz, params) []
  end >|=
  (function
   | Ok _ as r -> r
   | Error err -> Error (Caqti_error.show err))

let handle_count_messages' req =
  let {room = room_jid; pattern; tz} : count_messages_request = req in
  let room = Data.Node.of_string room_jid in (* TODO: exception *)
  let*? room_id =
    Data.Node.stored_id room >|= function
     | None -> Fmt.error "The room %s does not exist." room_jid
     | Some id -> Ok id
  in
  (match
    let open Batyr_core.Search_sql.Expr in
    (var "sender.node_id" = int room_id)
    |> Prime_option.fold (fun pat -> (&&) (sql_of_pattern pat)) pattern
   with
   | cond -> fetch_message_counts cond tz
   | exception Batyr_core.Search.Syntax_error msg -> Lwt.return (Error msg))

let handle_count_messages =
  make_handler
    count_messages_request_of_yojson
    count_messages_response_to_yojson
    handle_count_messages'

let fetch_messages cond =
  let cond_str, Batyr_core.Search_sql.Param (params_type, params) =
    Batyr_core.Search_sql.Expr.to_sql cond in
  let req =
    let open Caqti_type.Std in
    let open Caqti_request.Infix in
    (params_type ->*
     tup2 (tup3 ptime (option ptime) int)
          (tup3 (option string) (option string) (option string)))
      ~oneshot:true
    (sprintf
      "SELECT seen_time, edit_time, sender_id, subject, thread, body \
       FROM batyr.messages \
       JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
         ON sender_id = sender.resource_id \
       WHERE %s \
       ORDER BY seen_time, message_id LIMIT %d"
      cond_str query_limit) in
  (Data.Db.use @@ fun (module C : Data.CONNECTION) ->
    C.fold req List.cons params []) >>=
  (function
   | Ok tuples ->
      Lwt_list.rev_map_p
        (fun ((time, edit_time, sender_id),
              (subject_opt, thread_opt, body_opt)) ->
          Data.Resource.stored_of_id sender_id >|= fun sender_resource ->
          { msg_time = time;
            msg_edit_time = edit_time;
            msg_sender_cls = "jid";
            msg_sender = Data.Resource.resource_name sender_resource;
            msg_subject = subject_opt;
            msg_thread = thread_opt;
            msg_body = body_opt })
        tuples >|= (fun msgs -> Ok msgs)
   | Error err -> Lwt.return_error (Caqti_error.show err))

let phrase_query pattern tI_opt tF_opt =
    (match pattern with None -> "" | Some pat -> " matching " ^ pat)
  ^ (match tI_opt with None -> "" | Some tI -> sprintf " from %f" tI)
  ^ (match tF_opt with None -> "" | Some tF -> sprintf " to %f" tF)

let handle_fetch_messages' req =
  (* TODO: time zone *)
  let {room = room_jid; time_start; time_stop; pattern} = req in
  Log.debug (fun f ->
    f "Sending %s transcript%s."
      room_jid (phrase_query pattern time_start time_stop)) >>= fun () ->
  let room = Data.Node.of_string room_jid in (* TODO: exception *)
  let*? room_id =
    Data.Node.stored_id room >|= function
     | None -> Fmt.error "The room %s does not exist." room_jid
     | Some id -> Ok id
  in
  (match
    let open Batyr_core.Search_sql.Expr in
    (var "sender.node_id" = int room_id)
    |> Prime_option.fold (fun t -> (&&) (var "seen_time" >= epoch t)) time_start
    |> Prime_option.fold (fun t -> (&&) (var "seen_time" < epoch t)) time_stop
    |> Prime_option.fold (fun pat -> (&&) (sql_of_pattern pat)) pattern
   with
   | cond -> fetch_messages cond
   | exception Batyr_core.Search.Syntax_error msg -> Lwt.return_error msg)

let handle_fetch_messages =
  make_handler
    fetch_messages_request_of_yojson
    fetch_messages_response_to_yojson
    handle_fetch_messages'
