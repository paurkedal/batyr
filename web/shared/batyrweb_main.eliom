(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

{shared{
  open Eliom_content
  open Printf
  open Unprime
  open Unprime_list
  open Unprime_option

  type chatroom = string
  let query_limit = 100
}}
{server{
  open Batyr_data
  open Batyr_prereq
  open Batyrweb_server
}}
{client{
  open Batyrweb_client
  module Chatroom = struct
    type t = chatroom
    let compare = String.compare
    let render_row room = Html5.D.([td [pcdata room]])
  end
  module Chatrooms_live = Live_table (Chatroom)
}}

let main_handler () () =
  lwt rooms =
    Batyr_db.use (fun dbh ->
      dbh#query_list Batyr_db.Decode.int
	"SELECT DISTINCT node_id \
	 FROM batyr.muc_rooms NATURAL JOIN batyr.nodes \
			      NATURAL JOIN batyr.domains") in
  let render_room_link node_id =
    lwt node = Node.of_id node_id in
    let node_jid = Node.to_string node in
    Lwt.return Html5.D.(li [a ~service:transcript_service [pcdata node_jid]
			      (node_jid, (0.0, (None, None)))]) in
  lwt room_lis = Lwt_list.map_p render_room_link rooms in
  let rooms_ul = Html5.D.ul room_lis in
  Lwt.return (Batyrweb_tools.D.page "Chatrooms" [rooms_ul])

{shared{
  type message = {
    msg_time : float;
    msg_sender_cls : string;
    msg_sender : string;
    msg_subject : string option;
    msg_thread : string option;
    msg_body : string option;
  }
}}

let client_transcript_service =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.
      (string "room" ** float "tI" ** opt (float "tF") ** opt (string "pat"))
    begin fun (room_jid, (tI, (tF_opt, pat_opt))) () -> Lwt.catch
      begin fun () ->
	Lwt_log.debug_f "Requesting transcript for %s from %g." room_jid tI >>
	lwt room = Lwt.wrap1 Node.of_string room_jid in
	lwt room_id = Node.id room in
	let cond =
	  Batyr_db.Expr.(
	    (var "sender.node_id" = int room_id
	      && epoch tI <= var "seen_time")
	    |> Option.fold (fun tF -> (&&) (var "seen_time" < epoch tF)) tF_opt
	    |> Option.fold (fun pat -> (&&) (var "subject" =~ pat ||
					     var "thread" =~ pat ||
					     var "body" =~ pat)) pat_opt
	  ) in
	let cond_str, params = Batyr_db.Expr.to_sql cond in
	Batyr_db.use
	  (fun dbh ->
	    dbh#query_list ~params
	      Batyr_db.Decode.(epoch ** int ** option string **
			       option string ** option string)
	      (sprintf "SELECT seen_time, sender_id, subject, thread, body \
			FROM batyr.messages \
			JOIN (batyr.peers NATURAL JOIN batyr.nodes) \
			  AS sender \
			  ON sender_id = sender.peer_id \
			WHERE %s \
			ORDER BY seen_time, message_id LIMIT %d"
		       cond_str query_limit)) >>=
	Lwt_list.map_p
	  (fun (time, (sender_id, (subject_opt, (thread_opt, body_opt)))) ->
	    Peer.of_id sender_id >|= fun sender_peer ->
	    { msg_time = time;
	      msg_sender_cls = "jid";
	      msg_sender = Peer.resource sender_peer;
	      msg_subject = subject_opt;
	      msg_thread = thread_opt;
	      msg_body = body_opt })
      end
      begin fun xc ->
	let msg = Printexc.to_string xc in
	Lwt_log.error_f "Failed query of transcript list: %s" msg >>
	Lwt.return [{
	  msg_time = Unix.time ();
	  msg_sender_cls = "meta";
	  msg_sender = "system";
	  msg_subject = Some "System Error";
	  msg_thread = None;
	  msg_body = Some (sprintf "Query failed: %s" msg);
	}]
      end
    end

{client{
  let month_names = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
		      "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]
  let day_names = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]

  let render_messages ~room ~tI ?tF ?pat () =
    Eliom_client.call_caml_service ~service:%client_transcript_service
				   (room, (tI, (tF, pat))) ()
      >|= fun messages ->
    let current_day = ref (0, 0, 0) in
    let transcript_div = Html5.D.div [] in
    let transcript_dom = Html5.To_dom.of_div transcript_div in
    List.iter
      (fun msg ->
	let open Html5.F in
	let msg_frag =
	  match msg.msg_body with
	  | None -> []
	  | Some body -> [span ~a:[a_class ["body"]] [pcdata body]] in
	let msg_frag =
	  match
	    match msg.msg_subject, msg.msg_thread with
	    | None, None -> None
	    | None, Some thread ->
	      Some [pcdata (sprintf "[- %s] " thread)]
	    | Some subject, None ->
	      Some [pcdata (sprintf "[%s] " subject)]
	    | Some subject, Some thread ->
	      Some [pcdata (sprintf "[%s - %s] " subject thread)]
	  with
	  | None -> msg_frag
	  | Some st_frag ->
	    span ~a:[a_class ["subject"]] st_frag :: msg_frag in
	let jstime = jsnew Js.date_fromTimeValue(msg.msg_time *. 1000.0) in
	let day = jstime##getFullYear(), jstime##getMonth() + 1,
		  jstime##getDate() in
	if day <> !current_day then begin
	  let (y, m, d), wd = day, day_names.(jstime##getDay()) in
	  let day_str = sprintf "%s %04d-%02d-%02d" wd y m d in
	  let header_h2 = h2 [pcdata day_str] in
	  Dom.appendChild transcript_dom (Html5.To_dom.of_h2 header_h2);
	  current_day := day
	end;
	let hour =
	  sprintf "%02d:%02d:%02d"
	    jstime##getHours() jstime##getMinutes() jstime##getSeconds() in
	let message_p =
	  (p ~a:[a_class ["message"]]
	    (span ~a:[a_class ["hour"]] [pcdata hour] :: pcdata " " ::
	     span ~a:[a_class ["sender"; msg.msg_sender_cls]]
		  [pcdata msg.msg_sender] ::
	     pcdata ": " ::
	     msg_frag)) in
	Dom.appendChild transcript_dom (Html5.To_dom.of_p message_p))
      messages;
    [transcript_div]

  let day_interval y m d =
    let dI = jsnew Js.date_day(y, m - 1, d) in
    let dF = jsnew Js.date_day(y, m - 1, d + 1) in
    (0.001 *. Js.to_float dI##valueOf(), 0.001 *. Js.to_float dF##valueOf())

  let month_interval y m =
    let dI, dF = jsnew Js.date_month(y, m - 1), jsnew Js.date_month(y, m + 1) in
    (0.001 *. Js.to_float dI##valueOf(), 0.001 *. Js.to_float dF##valueOf())

  let year_interval y =
    let dI, dF = jsnew Js.date_month(y, 0), jsnew Js.date_month(y + 1, 0) in
    (0.001 *. Js.to_float dI##valueOf(), 0.001 *. Js.to_float dF##valueOf())

  let update_transcript_view ~room ~min_time ?pat transcript_dom =
    let jt_min = jsnew Js.date_fromTimeValue(min_time *. 1000.0) in
    let jt_now = jsnew Js.date_now() in
    let y_min = jt_min##getFullYear() in
    let y_now = jt_now##getFullYear() in

    let render_content y m d _ =
      let tI, tF = day_interval y m d in
      Eliom_lib.debug "Selected %04d-%02d-%02d = [%f, %f)." y m d tI tF;
      render_messages ~room ~tI ~tF ?pat () in

    let render_mdays y m = function
      | [] ->
	let tI, tF = month_interval y m in
	render_messages ~room ~tI ~tF ?pat ()
      | [d] ->
	let last_day_of_m = jsnew Js.date_day(y, m + 1, -1) in
	let days_in_m = last_day_of_m##getDate() + 1 in
	let mdays = Array.init days_in_m (fun i -> sprintf "%d" (i + 1)) in
	Batyrweb_tools.D.pager ~default_index:(d - 1) mdays
			       (fun i -> render_content y m (i + 1))
      | _ -> assert false in
    let render_months y = function
      | [] ->
	let tI, tF = year_interval y in
	render_messages ~room ~tI ~tF ?pat ()
      | m :: d_ ->
	Batyrweb_tools.D.pager ~default_index:(m - 1) month_names
			       (fun i _ -> render_mdays y (i + 1) d_) in
    let render_years = function
      (* | [] -> render_messages ~room ~tI ?pat () *)
      | [] -> assert false
      | y :: m_d_ ->
	let years = Array.init (y_now - y_min + 1)
			       (fun dy -> sprintf "%04d" (y_min + dy)) in
	Batyrweb_tools.D.pager ~default_index:(y - y_min) years
			       (fun dy _ -> render_months (y_min + dy) m_d_) in
    transcript_dom##innerHTML <- Js.string "";
    render_years [y_now; 1; 1] >|=
    List.iter
      (fun el -> Dom.appendChild transcript_dom (Html5.To_dom.of_element el))
}}

let transcript_handler (room_jid, (tI, (tF, pat))) () =
  let transcript_div = Html5.D.(div ~a:[a_class ["transcript"]] []) in
  let room_node = Node.of_string room_jid in
  lwt room = Muc_room.of_node room_node in
  let min_time = Option.get_else Sys.time (Muc_room.min_message_time room) in
  ignore {unit{
    let transcript_dom = Html5.To_dom.of_div %transcript_div in
    let tI = ref %tI in
    let tF = ref %tF in
    let pat = ref %pat in
    Lwt.ignore_result begin
      update_transcript_view ~room:%room_jid ~min_time:%min_time ?pat:!pat
			     transcript_dom
    end
  }};
  Lwt.return (Batyrweb_tools.D.page
    (sprintf "Transcript of %s" room_jid)
    [transcript_div]
  )

module Main_app =
  Eliom_registration.App (struct let application_name = "web-batyrweb_main" end)
let () =
  Main_app.register ~service:main_service main_handler;
  Main_app.register ~service:transcript_service transcript_handler
