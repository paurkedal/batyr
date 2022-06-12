(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%server
  open Batyr_core.Data
  open Batyrweb_server
  module type CONNECTION = Caqti_lwt.CONNECTION
  module Caqti_type = struct
    include Caqti_type
    include Caqti_type_calendar
  end
  module B = Batyr_xmpp_conn
]
[%%client
  open Js_of_ocaml
  open Batyrweb_client
]
[%%shared
  open Eliom_client
  open Eliom_content.Html
  open Eliom_lib
  open Lwt.Infix
  open Printf
  open Scanf
  open Unprime
  open Unprime_char
  open Unprime_list
  open Unprime_option
  open Unprime_string
]

let%shared section = Lwt_log.Section.make "batyrweb.main"
type%shared chatroom = string
let%shared query_limit = 10000

module%client Chatroom = struct
  type t = chatroom
  let compare = String.compare
  let render_row room = D.([td [txt room]])
end
module%client Chatrooms_live = Live_table (Chatroom)

let%client fragment =
  let sg, set = React.S.create (Url.Current.get_fragment ()) in
  Dom_html.window##.onhashchange :=
    Dom.handler (fun _ -> set (Url.Current.get_fragment ()); Js._true);
  sg

let%client fragment_date = React.S.map
  (fun frag ->
    let frag_date =
      match String.cut_affix "T" frag with Some (s, _) -> s | None -> frag in
    match String.chop_affix "-" frag_date with
    | [sY; sM; sD] ->
      (try Some (new%js Js.date_day (int_of_string sY) (int_of_string sM - 1)
                                    (int_of_string sD))
       with Failure _ -> None)
    | _ -> None)
  fragment

type%shared message = {
  msg_time : Ptime.t;
  msg_edit_time : Ptime.t option;
  msg_sender_cls : string;
  msg_sender : string;
  msg_subject : string option;
  msg_thread : string option;
  msg_body : string option;
}

let phrase_query pat_opt tI_opt tF_opt =
    (match pat_opt with None -> "" | Some pat -> " matching " ^ pat)
  ^ (match tI_opt with None -> "" | Some tI -> sprintf " from %f" tI)
  ^ (match tF_opt with None -> "" | Some tF -> sprintf " to %f" tF)

let sql_of_pattern pat_s =
  Batyr_core.Search.(denote_pattern (pattern_of_string pat_s))

let fetch_message_counts (room_jid, pat_opt, tz) =
  try%lwt
    let%lwt room = Lwt.wrap1 B.Node.of_string room_jid in
    let%lwt room_id =
      match%lwt B.Node.stored_id room with
      | None -> Lwt.fail Eliom_common.Eliom_404
      | Some id -> Lwt.return id in
    let cond =
      let open Batyr_core.Search_sql.Expr in
      var "sender.node_id" = int room_id
      |> Option.fold (fun pat -> (&&) (sql_of_pattern pat)) pat_opt in
    let cond_str, Batyr_core.Search_sql.Param (params_type, params) =
      Batyr_core.Search_sql.Expr.to_sql ~first_index:2 cond in
    let q =
      let open Caqti_type.Std in
      let open Caqti_request.Infix in
      (tup2 string params_type ->* tup2 int int) ~oneshot:true
      (sprintf
        "SELECT batyr.intenc_date(seen_time AT TIME ZONE 'UTC' \
                                            AT TIME ZONE $1) AS t, \
                count(0) \
         FROM batyr.messages \
         JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
           ON sender_id = sender.resource_id \
         WHERE %s GROUP BY t"
         cond_str) in
    B.Db.use
      (fun (module C : CONNECTION) -> C.fold q List.cons (tz, params) []) >|=
    (function
     | Ok _ as r -> r
     | Error err -> Error (Caqti_error.show err))
  with
   | Batyr_core.Search.Syntax_error msg ->
      Lwt.return (Error msg)
   | exn ->
      Lwt_log.error_f ~exn "Failed query of transcript list." >>= fun () ->
      Lwt.return (Error "Query failed, see log for details.")

let%client fetch_message_counts =
  ~%(server_function [%json: string * string option * string]
                     fetch_message_counts)

let fetch_transcript (room_jid, tI_opt, tF_opt, pat_opt) =
  try%lwt
    Lwt_log.debug_f ~section "Sending %s transcript%s."
                    room_jid (phrase_query pat_opt tI_opt tF_opt) >>= fun () ->
    let%lwt room = Lwt.wrap1 B.Node.of_string room_jid in
    let%lwt room_id =
      match%lwt B.Node.stored_id room with
      | None -> Lwt.fail Eliom_common.Eliom_404
      | Some id -> Lwt.return id in
    let cond =
      Batyr_core.Search_sql.Expr.(
        (var "sender.node_id" = int room_id)
        |> Option.fold (fun tI -> (&&) (var "seen_time" >= epoch tI)) tI_opt
        |> Option.fold (fun tF -> (&&) (var "seen_time" < epoch tF)) tF_opt
        |> Option.fold (fun pat -> (&&) (sql_of_pattern pat)) pat_opt
      ) in
    let cond_str, Batyr_core.Search_sql.Param (params_type, params) =
      Batyr_core.Search_sql.Expr.to_sql cond in
    let q =
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
    (B.Db.use @@ fun (module C : CONNECTION) ->
      C.fold q List.cons params []) >>=
    (function
     | Ok tuples ->
        Lwt_list.rev_map_p
          (fun ((time, edit_time, sender_id),
                (subject_opt, thread_opt, body_opt)) ->
            B.Resource.stored_of_id sender_id >|= fun sender_resource ->
            { msg_time = time;
              msg_edit_time = edit_time;
              msg_sender_cls = "jid";
              msg_sender = B.Resource.resource_name sender_resource;
              msg_subject = subject_opt;
              msg_thread = thread_opt;
              msg_body = body_opt })
          tuples >|= (fun msgs -> Ok msgs)
     | Error err -> Lwt.return_error (Caqti_error.show err))
  with
   | Batyr_core.Search.Syntax_error msg ->
      Lwt.return (Error msg)
   | exn ->
      Lwt_log.error_f ~exn "Failed query of transcript list." >>= fun () ->
      Lwt.return (Error "Query failed, see log for details.")

let%client fetch_transcript =
  ~%(server_function
      [%json: string * float option * float option * string option]
      fetch_transcript)

module%client Message_counts = struct

  type t = {
    mutable card : int;
    mutable branches : t array;
  }

  let card mc = mc.card

  let year_span mc = Array.length mc.branches

  let get mc i = mc.branches.(i)

  let fetch room pat y_min y_now tz =
    let mc =
      let init_mday d = {card = 0; branches = [||]} in
      let init_month m = {card = 0; branches = Array.init 31 init_mday} in
      let init_year _ = {card = 0; branches = Array.init 12 init_month} in
      {card = 0; branches = Array.init (y_now - y_min + 1) init_year} in

    fetch_message_counts (room, pat, tz) >|= function
     | Ok counts ->
        List.iter
          (fun (encdate, count) ->
            let y = encdate lsr 16 in
            let m = encdate lsr 8 land 0xff in
            let d = encdate land 0xff in
            let mcY = mc.branches.(y - y_min) in
            let mcM = mcY.branches.(m - 1) in
            let mcD = mcM.branches.(d - 1) in
            mcD.card <- count;
            mcM.card <- mcM.card + count;
            mcY.card <- mcY.card + count)
          counts;
        Ok mc
     | Error msg -> Error msg
end

module%client Transcript = struct

  type t = {
    mutable ts_day : int * int * int;
    mutable ts_day_str : string;
    ts_dom : Dom_html.divElement Js.t;
    mutable ts_updater : unit Lwt.t option;
  }

  let create () =
    let transcript_div = D.div [] in
    let ts = {
      ts_day = (0, 0, 0);
      ts_day_str = "";
      ts_dom = To_dom.of_div transcript_div;
      ts_updater = None;
    } in
    (transcript_div, ts)

  let append_message ts msg =
    let msg_frag =
      match msg.msg_body with
      | None -> []
      | Some body -> [F.span ~a:[F.a_class ["body"]] [F.txt body]] in
    let msg_frag =
      match
        match msg.msg_subject, msg.msg_thread with
        | None, None -> None
        | None, Some thread ->
          Some [F.txt (sprintf "[- %s] " thread)]
        | Some subject, None ->
          Some [F.txt (sprintf "[%s] " subject)]
        | Some subject, Some thread ->
          Some [F.txt (sprintf "[%s - %s] " subject thread)]
      with
      | None -> msg_frag
      | Some st_frag ->
        F.span ~a:[F.a_class ["subject"]] st_frag :: msg_frag in
    let msg_frag =
      (match msg.msg_edit_time with
       | None -> msg_frag
       | Some _ ->
          msg_frag @ [
            F.txt " ";
            F.span ~a:[F.a_class ["edited"]] [F.txt "(edited)"]
          ]) in
    let jstime = new%js Js.date_fromTimeValue
      ((Ptime.to_float_s msg.msg_time) *. 1000.0) in
    let day = jstime##getFullYear, jstime##getMonth + 1, jstime##getDate in
    if day <> ts.ts_day then begin
      let (y, m, d), wd = day, Caltime.day_names.(jstime##getDay) in
      let day_str = sprintf "%04d-%02d-%02d" y m d in
      let header_h2 = F.h2 [F.txt (wd ^ " " ^ day_str)] in
      Dom.appendChild ts.ts_dom (To_dom.of_h2 header_h2);
      ts.ts_day <- day;
      ts.ts_day_str <- day_str
    end;
    let h, m, s = jstime##getHours, jstime##getMinutes, jstime##getSeconds in
    let hour = sprintf "%02d:%02d:%02d" h m s in
    let frag = sprintf "%sT%02d%02d%02d" ts.ts_day_str h m s in
    let message_p =
      (F.p ~a:[F.a_class ["message"]; F.a_id frag]
        (F.span ~a:[F.a_class ["hour"]] [F.txt hour] :: F.txt " " ::
         F.span ~a:[F.a_class ["sender"; msg.msg_sender_cls]]
                [F.txt msg.msg_sender] ::
         F.txt ": " ::
         msg_frag)) in
    Dom.appendChild ts.ts_dom (To_dom.of_p message_p)

  let append_messages ts msgs =
    let message_count = List.length msgs in
    List.iter (append_message ts) msgs;
    if message_count = query_limit then begin
      let limit_s =
        sprintf "This result has been limited to %d messages." query_limit in
      let limit_p = D.(p ~a:[a_class ["warning"]] [txt limit_s]) in
      Dom.appendChild ts.ts_dom (To_dom.of_p limit_p)
    end

  let enable_updates ts update_comet =
    if ts.ts_updater = None then begin
      Eliom_lib.debug "Enabling updates.";
      ts.ts_updater <- Some (Lwt_stream.iter (append_message ts) update_comet)
    end

  let disable_updates ts =
    match ts.ts_updater with
     | None -> ()
     | Some updater ->
        Eliom_lib.debug "Disabling updates.";
        ts.ts_updater <- None;
        Lwt.cancel updater
end

let%client shown_transcript = ref None

let%client render_transcript ~room ?tI ?tF ?pat ?date update_comet =
  begin match !shown_transcript with
   | None -> ()
   | Some ts -> Transcript.disable_updates ts; shown_transcript := None
  end;
  fetch_transcript (room, tI, tF, pat) >|= function
   | Error msg ->
      [F.p ~a:[F.a_class ["error"]] [F.txt msg]]
   | Ok msgs ->
      let content, ts = Transcript.create () in
      Transcript.append_messages ts msgs;

      let is_past date =
        let jt_now = new%js Js.date_now in
        Caltime.day_start date <> Caltime.day_start jt_now in
      if not (Option.for_all is_past date) then
        Transcript.enable_updates ts update_comet;

      shown_transcript := Some ts;
      [content]

type%client shown_counts = {
  sc_room : string;
  sc_pattern : string option;
  sc_counts : Message_counts.t;
}
let%client shown_counts = ref None

let%client render_page ~room ~min_time ?pat ~page transcript_dom update_comet =
  let jt_min = new%js Js.date_fromTimeValue (min_time *. 1000.0) in
  let jt_now = new%js Js.date_now in
  let y_min = jt_min##getFullYear in
  let y_now = jt_now##getFullYear in

  let render_day iY iM iD =
    let date = new%js Js.date_day (y_min + iY) iM (1 + iD) in
    let tI = Caltime.to_epoch (Caltime.day_start date) in
    let tF = Caltime.to_epoch (Caltime.day_end date) in
    page := [iY; iM; iD];
    render_transcript ~room ~tI ~tF ?pat ~date update_comet in

  let render_month mcY iY iM = function
   | [] ->
      let date = new%js Js.date_month (y_min + iY) iM in
      let tI = Caltime.to_epoch (Caltime.month_start date) in
      let tF = Caltime.to_epoch (Caltime.month_end date) in
      page := [iY; iM];
      render_transcript ~room ~tI ~tF ?pat ~date update_comet
   | [iD] ->
      let date = new%js Js.date_month (y_min + iY) iM in
      let mcM = Message_counts.get mcY iM in
      let label iD = sprintf "%d" (iD + 1) in
      Batyrweb_content_js.pager
        ~default_index:iD
        ~count:(fun iD -> Message_counts.(card (get mcM iD)))
        (Array.init (Caltime.days_in_month date) label)
        (fun iD -> render_day iY iM iD)
   | _ -> assert false in

  let render_year mcA iY = function
   | [] ->
      let tI = Caltime.to_epoch (new%js Js.date_month (y_min + iY) 0) in
      let tF = Caltime.to_epoch (new%js Js.date_month (y_min + iY + 1) 0) in
      page := [iY];
      render_transcript ~room ~tI ~tF ?pat update_comet
   | iM :: iDs ->
      let mcY = Message_counts.get mcA iY in
      Batyrweb_content_js.pager
        ~default_index:iM
        ~count:(fun iM -> Message_counts.(card (get mcY iM)))
        Caltime.month_names
        (fun iM -> render_month mcY iY iM iDs) in

  let render_all mcA = function
   | [] ->
      render_transcript ~room ?pat update_comet
   | iY :: iMDs ->
      let label iY = sprintf "%04d" (y_min + iY) in
      Batyrweb_content_js.pager
        ~default_index:iY
        ~count:(fun iY -> Message_counts.(card (get mcA iY)))
        (Array.init (y_now - y_min + 1) label)
        (fun iY -> render_year mcA iY iMDs) in

  let is_current =
    match !shown_counts with
     | Some sc ->
        sc.sc_room = room && sc.sc_pattern = pat
         && Message_counts.year_span sc.sc_counts >= (y_now - y_min + 1)
     | None -> false in

  if is_current then Lwt.return (Ok ()) else begin
    (* FIXME: Need the real timezone for this to work across DST. *)
    let today = new%js Js.date_now in
    let tz = sprintf "%d" (today##getTimezoneOffset / 60) in
    match%lwt Message_counts.fetch room pat y_min y_now tz with
     | Ok counts ->
        let%lwt content = render_all counts !page in
        shown_counts := Some {
          sc_room = room;
          sc_pattern = pat;
          sc_counts = counts;
        };
        transcript_dom##.innerHTML := Js.string "";
        List.iter (Dom.appendChild transcript_dom % To_dom.of_element) content;
        Lwt.return (Ok ())
     | Error msg ->
        Lwt.return (Error msg)
  end

let transcript_handler (room_jid, pat) () =
  let open D in
  let transcript_div = div ~a:[a_class ["transcript"]] [] in
  let room_node = B.Node.of_string room_jid in
  let%lwt room =
    match%lwt B.Muc_room.stored_of_node room_node with
    | None -> Lwt.fail Eliom_common.Eliom_404
    | Some room -> Lwt.return room in
  let min_time = Option.get_else Unix.time (B.Muc_room.min_message_time room) in
  let page : int list ref Eliom_client_value.t = [%client
    let jt_min = new%js Js.date_fromTimeValue (~%min_time *. 1000.0) in
    let y_min = jt_min##getFullYear in
    let d = Option.get_else (fun () -> new%js Js.date_now)
                            (React.S.value fragment_date) in
    ref [d##getFullYear - y_min; d##getMonth; d##getDate - 1]
  ] in
  let relevant_message msg =
    let open Batyr_xmpp_listener in
    Lwt.return begin
      if B.Resource.node (B.Message.sender msg) != room_node then None else
      Some {
        msg_time = B.Message.seen_time msg;
        msg_edit_time = B.Message.edit_time msg;
        msg_sender_cls = "jid";
        msg_sender = B.Resource.resource_name (B.Message.sender msg);
        msg_subject = B.Message.subject msg;
        msg_thread = B.Message.thread msg;
        msg_body = B.Message.body msg;
      }
    end in
  let update_events =
    Lwt_react.E.fmap_s relevant_message Batyr_xmpp_listener.messages in
  let update_comet =
    Eliom_comet.Channel.create (Lwt_react.E.to_stream update_events) in
  let info_span = span ~a:[a_class ["error"]] [] in
  let clear_handler =
    [%client fun ev ->
      let info_dom = To_dom.of_span ~%(info_span : [`Span] elt) in
      let search_dom =
        Domx_html.element_by_id Dom_html.CoerceTo.input "search_text" in
      info_dom##.className := Js.string "";
      info_dom##.innerHTML := Js.string "";
      search_dom##.value := Js.string "";
      let clear_dom =
        Domx_html.element_by_id Dom_html.CoerceTo.button "clear_search" in
      clear_dom##.disabled := Js._true;
      Lwt.ignore_result begin
        render_page
          ~room:~%room_jid ~min_time:~%min_time ~page:~%page
          (To_dom.of_div ~%(transcript_div : [`Div] elt)) ~%update_comet
      end
    ] in
  let clear_button =
    button ~a:[a_button_type `Button; a_onclick clear_handler;
               a_id "clear_search"; a_disabled ()]
           [txt "all"] in
  let search_handler =
    [%client fun ev ->
      let info_dom = To_dom.of_span ~%(info_span : [`Span] elt) in
      let search_dom =
        Domx_html.element_by_id Dom_html.CoerceTo.input "search_text" in
      let pat = match Js.to_string search_dom##.value
                with "" -> None | s -> Some s in
      (To_dom.of_button ~%(clear_button : [`Button] elt))##.disabled
        := Js.bool (pat = None);
      info_dom##.className := Js.string "";
      info_dom##.innerHTML := Js.string "";
      Lwt.async begin fun () ->
        match%lwt
          render_page
            ~room:~%room_jid ~min_time:~%min_time ?pat ~page:~%page
            (To_dom.of_div ~%(transcript_div : [`Div] elt))
            ~%update_comet
        with
         | Ok () -> Lwt.return_unit
         | Error msg ->
            info_dom##.className := Js.string "error";
            info_dom##.innerHTML := Js.string msg;
            Lwt.return_unit
      end
    ] in
  let search_handler_mouse =
    [%client fun ev -> ~%search_handler (ev :> Dom_html.event Js.t)] in
  let search_button =
    button ~a:[a_button_type `Button; a_onclick search_handler_mouse]
           [txt "matching"] in
  let search_input =
    input ~a:[a_input_type `Text; a_id "search_text"; a_onchange search_handler]
          () in
  ignore_client_unit [%client
    let transcript_dom = To_dom.of_div ~%(transcript_div : [`Div] elt) in
    let pat = ref ~%pat in
    Lwt.ignore_result begin
      render_page
        ~room:~%room_jid ~min_time:~%min_time ?pat:!pat ~page:~%page
        transcript_dom ~%update_comet
    end
  ];
  let help_span =
    let bop s = span ~a:[a_class ["bnf-op"]] [txt s] in
    let op s = span [bop "'"; txt s; bop "'"] in
    let vn s = span ~a:[a_style "font-style: italic"] [txt s] in
    let sp = txt " " in
    span ~a:[a_class ["help"]] [
      vn"pat"; sp; bop"::="; sp;
      bop"("; op"author:"; sp; bop"|"; sp; op"subject:"; sp; bop"|";
              op"body:"; bop")"; bop"?"; sp;
      bop"("; vn"word"; sp; bop"|"; sp;
              op"\""; vn"string"; op"\""; sp; bop")"; sp; bop"|"; sp;
      op"("; vn"pat"; op")"; sp; bop"|"; sp;
      op"!"; vn"pat"; sp; bop"|"; sp;
      vn"pat"; sp; vn"pat"; sp; bop"|"; sp;
      vn"pat"; sp; op"|"; sp; vn"pat"
    ] in
  Lwt.return (Batyrweb_content.page
    (sprintf "Transcript of %s" room_jid)
    [ div
      [ txt "Show "; clear_button;
        txt " or "; search_button; search_input; txt " ";
        info_span; txt " "; help_span ];
      transcript_div ]
  )

let () = Main_app.register ~service:transcript_service transcript_handler
