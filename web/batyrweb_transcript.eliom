(* Copyright (C) 2013--2015  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Scanf
  open Unprime
  open Unprime_char
  open Unprime_list
  open Unprime_option
  open Unprime_string

  type chatroom = string
  let query_limit = 10000
}}
{server{
  open Batyr_data
  open Batyr_prereq
  open Batyrweb_server
  open Caqti_lwt

  let section = Lwt_log.Section.make "batyrweb.main"
}}
{client{
  open Batyrweb_client
  module Chatroom = struct
    type t = chatroom
    let compare = String.compare
    let render_row room = Html5.D.([td [pcdata room]])
  end
  module Chatrooms_live = Live_table (Chatroom)

  let fragment =
    let sg, set = React.S.create (Url.Current.get_fragment ()) in
    Dom_html.window##onhashchange <-
      Dom.handler (fun _ -> set (Url.Current.get_fragment ()); Js._true);
    sg

  let fragment_date = React.S.map
    (fun frag ->
      let frag_date =
	match String.cut_affix "T" frag with Some (s, _) -> s | None -> frag in
      match String.chop_affix "-" frag_date with
      | [sY; sM; sD] ->
	(try Some (jsnew Js.date_day(int_of_string sY, int_of_string sM - 1,
				     int_of_string sD))
	 with Failure _ -> None)
      | _ -> None)
    fragment
}}

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

let phrase_query pat_opt tI_opt tF_opt =
    (match pat_opt with None -> "" | Some pat -> " matching " ^ pat)
  ^ (match tI_opt with None -> "" | Some tI -> sprintf " from %f" tI)
  ^ (match tF_opt with None -> "" | Some tF -> sprintf " to %f" tF)

let sql_of_pattern pat_s =
  Batyr_search.denote_pattern (Batyr_search.pattern_of_string pat_s)

let client_message_counts_service =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.(string "room" ** opt (string "pat") **
				 string "tz")
    begin fun (room_jid, (pat_opt, tz)) () ->
      lwt room = Lwt.wrap1 Node.of_string room_jid in
      lwt room_id =
	match_lwt Node.stored_id room with
	| None -> Lwt.fail Eliom_common.Eliom_404
	| Some id -> Lwt.return id in
      let cond =
	let open Batyr_db.Expr in
	var "sender.node_id" = int room_id
	|> Option.fold (fun pat -> (&&) (sql_of_pattern pat)) pat_opt in
      let cond_str, params = Batyr_db.Expr.to_sql ~first_index:2 cond in
      let q = Caqti_query.oneshot_fun @@ function
	`Pgsql ->
	  sprintf
	    "SELECT batyr.intenc_date(seen_time AT TIME ZONE 'UTC' \
						AT TIME ZONE $1) AS t, \
		    count(0) \
	     FROM batyr.messages \
	     JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
	       ON sender_id = sender.resource_id \
	     WHERE %s GROUP BY t"
	     cond_str
	| _ -> raise Caqti_query.Missing_query_string in
      Batyr_db.use @@ fun (module C : CONNECTION) ->
	C.fold q C.Tuple.(fun t -> List.push (int 0 t, int 1 t))
	       (Array.map C.Param.string (Array.append [|tz|] params)) []
    end

let client_transcript_service =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.
      (string "room" ** opt (float "tI") ** opt (float "tF") **
       opt (string "pat"))
    begin fun (room_jid, (tI_opt, (tF_opt, pat_opt))) () -> Lwt.catch
      begin fun () ->
	Lwt_log.debug_f ~section "Sending %s transcript%s."
			room_jid (phrase_query pat_opt tI_opt tF_opt) >>
	lwt room = Lwt.wrap1 Node.of_string room_jid in
	lwt room_id =
	  match_lwt Node.stored_id room with
	  | None -> Lwt.fail Eliom_common.Eliom_404
	  | Some id -> Lwt.return id in
	let cond =
	  Batyr_db.Expr.(
	    (var "sender.node_id" = int room_id)
	    |> Option.fold (fun tI -> (&&) (var "seen_time" >= epoch tI)) tI_opt
	    |> Option.fold (fun tF -> (&&) (var "seen_time" < epoch tF)) tF_opt
	    |> Option.fold (fun pat -> (&&) (sql_of_pattern pat)) pat_opt
	  ) in
	let cond_str, params = Batyr_db.Expr.to_sql cond in
	let q = Caqti_query.oneshot_fun @@ function
	  | `Pgsql ->
	    (sprintf "SELECT seen_time, sender_id, subject, thread, body \
			FROM batyr.messages \
			JOIN (batyr.resources NATURAL JOIN batyr.nodes) \
			  AS sender \
			  ON sender_id = sender.resource_id \
			WHERE %s \
			ORDER BY seen_time, message_id LIMIT %d"
		       cond_str query_limit)
	  | _ -> raise Caqti_query.Missing_query_string in
	(Batyr_db.use @@ fun (module C : CONNECTION) ->
	  C.fold q
	    C.Tuple.(fun t -> List.push (utc 0 t, int 1 t, option string 2 t,
					 option string 3 t, option string 4 t))
	    (Array.map C.Param.string params) []) >>=
	Lwt_list.rev_map_p
	  (fun (time, sender_id, subject_opt, thread_opt, body_opt) ->
	    Resource.stored_of_id sender_id >|= fun sender_resource ->
	    { msg_time = CalendarLib.Calendar.to_unixfloat time;
	      msg_sender_cls = "jid";
	      msg_sender = Resource.resource_name sender_resource;
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

  type transcript_state = {
    mutable ts_day : int * int * int;
    mutable ts_day_str : string;
    ts_dom : Dom_html.divElement Js.t;
  }

  let append_message ts msg =
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
    if day <> ts.ts_day then begin
      let (y, m, d), wd = day, Caltime.day_names.(jstime##getDay()) in
      let day_str = sprintf "%04d-%02d-%02d" y m d in
      let header_h2 = h2 [pcdata (wd ^ " " ^ day_str)] in
      Dom.appendChild ts.ts_dom (Html5.To_dom.of_h2 header_h2);
      ts.ts_day <- day;
      ts.ts_day_str <- day_str
    end;
    let h, m, s =
      jstime##getHours(), jstime##getMinutes(), jstime##getSeconds() in
    let hour = sprintf "%02d:%02d:%02d" h m s in
    let frag = sprintf "%sT%02d%02d%02d" ts.ts_day_str h m s in
    let message_p =
      (p ~a:[a_class ["message"]; a_id frag]
	(span ~a:[a_class ["hour"]] [pcdata hour] :: pcdata " " ::
	 span ~a:[a_class ["sender"; msg.msg_sender_cls]]
	      [pcdata msg.msg_sender] ::
	 pcdata ": " ::
	 msg_frag)) in
    Dom.appendChild ts.ts_dom (Html5.To_dom.of_p message_p)

  let update_thread = ref None

  let enable_updates ts update_comet =
    if !update_thread = None then begin
      Eliom_lib.debug "Enabling updates.";
      update_thread := Some (Lwt_stream.iter (append_message ts) update_comet)
    end

  let disable_updates () =
    Option.iter
      begin fun thread ->
	Eliom_lib.debug "Disabling updates.";
	update_thread := None;
	Lwt.cancel thread
      end
      !update_thread

  type card_tree = {
    mutable ct_card : int;
    mutable ct_branches : card_tree array;
  }

  let shown_prec = ref 3
  let shown_date = Option.get_else (fun () -> jsnew Js.date_now())
				   (React.S.value fragment_date)
  let shown_room = ref ""
  let shown_pat = ref None
  let shown_counts = {ct_card = 0; ct_branches = [||]}

  let update_message_counts room pat y_min y_now =
    if !shown_room = room && !shown_pat = pat
	  && Array.length shown_counts.ct_branches >= (y_now - y_min + 1)
    then
      Lwt.return_unit
    else begin
      (* FIXME: Need the real timezone for this to work across DST. *)
      let tz = sprintf "%d" (shown_date##getTimezoneOffset() / 60) in
      lwt counts =
	Eliom_client.call_ocaml_service ~service:%client_message_counts_service
	  (room, (pat, tz)) () in
      let init_mday d =
	{ct_card = 0; ct_branches = [||]} in
      let init_month m =
	{ct_card = 0; ct_branches = Array.init 31 init_mday} in
      let init_year _ =
	{ct_card = 0; ct_branches = Array.init 12 init_month} in
      shown_counts.ct_branches <- Array.init (y_now - y_min + 1) init_year;
      List.iter
	(fun (encdate, count) ->
	  let y = encdate lsr 16 in
	  let m = encdate lsr 8 land 0xff in
	  let d = encdate land 0xff in
	  let ct_year = shown_counts.ct_branches.(y - y_min) in
	  let ct_month = ct_year.ct_branches.(m - 1) in
	  let ct_day = ct_month.ct_branches.(d - 1) in
	  ct_day.ct_card   <- count;
	  ct_month.ct_card <- ct_month.ct_card + count;
	  ct_year.ct_card  <- ct_year.ct_card + count)
	counts;
      shown_room := room;
      shown_pat := pat;
      Lwt.return_unit
    end

  let render_messages ~room ?tI ?tF ?pat update_comet =
    disable_updates ();
    Eliom_client.call_ocaml_service ~service:%client_transcript_service
				    (room, (tI, (tF, pat))) ()
      >|= fun messages ->
    let transcript_div = Html5.D.div [] in
    let ts = {
      ts_day = (0, 0, 0);
      ts_day_str = "";
      ts_dom = Html5.To_dom.of_div transcript_div;
    } in
    let message_count = List.length messages in
    List.iter (append_message ts) messages;
    if message_count = query_limit then begin
      let limit_s =
	sprintf "This result has been limited to %d messages." query_limit in
      let limit_p = Html5.D.(p ~a:[a_class ["warning"]] [pcdata limit_s]) in
      Dom.appendChild ts.ts_dom (Html5.To_dom.of_p limit_p)
    end;
    let jt_now = jsnew Js.date_now() in
    if Caltime.day_start shown_date = Caltime.day_start jt_now then
      enable_updates ts update_comet;
    [transcript_div]

  let update_transcript_view ~room ~min_time ?pat transcript_dom update_comet =
    let jt_min = jsnew Js.date_fromTimeValue(min_time *. 1000.0) in
    let jt_now = jsnew Js.date_now() in
    let y_min = jt_min##getFullYear() in
    let y_now = jt_now##getFullYear() in

    lwt () = update_message_counts room pat y_min y_now in

    let render_content dd =
      let d = 1 + dd in
      ignore shown_date##setDate(d);
      let tI = Caltime.to_epoch (Caltime.day_start shown_date) in
      let tF = Caltime.to_epoch (Caltime.day_end shown_date) in
      render_messages ~room ~tI ~tF ?pat update_comet in

    let render_mdays ct_year dm =
      let ct_month = ct_year.ct_branches.(dm) in
      let m = 1 + dm in
      ignore shown_date##setMonth(m - 1);
      if !shown_prec = 2 then
	let tI = Caltime.to_epoch (Caltime.month_start shown_date) in
	let tF = Caltime.to_epoch (Caltime.month_end shown_date) in
	render_messages ~room ~tI ~tF ?pat update_comet
      else
	let d_dfl = shown_date##getDate() in
	let mdays = Array.init (Caltime.days_in_month shown_date)
			       (fun i -> sprintf "%d" (i + 1)) in
	Batyrweb_tools.D.pager
	  ~default_index:(d_dfl - 1)
	  ~count:(fun dd -> ct_month.ct_branches.(dd).ct_card)
	  mdays render_content in
    let render_months dy =
      let ct_year = shown_counts.ct_branches.(dy) in
      let y = y_min + dy in
      ignore shown_date##setFullYear(y);
      if !shown_prec = 1 then
	let tI = Caltime.to_epoch (Caltime.year_start shown_date) in
	let tF = Caltime.to_epoch (Caltime.year_end shown_date) in
	render_messages ~room ~tI ~tF ?pat update_comet
      else
	let m_dfl = shown_date##getMonth() + 1 in
	Batyrweb_tools.D.pager
	  ~default_index:(m_dfl - 1)
	  ~count:(fun dm -> ct_year.ct_branches.(dm).ct_card)
	  Caltime.month_names (render_mdays ct_year) in
    let render_years () =
      if !shown_prec = 0 then
	render_messages ~room ?pat update_comet
      else
	let y_dfl = shown_date##getFullYear() in
	let years = Array.init (y_now - y_min + 1)
			       (fun dy -> sprintf "%04d" (y_min + dy)) in
	Batyrweb_tools.D.pager
	  ~default_index:(y_dfl - y_min)
	  ~count:(fun dy -> shown_counts.ct_branches.(dy).ct_card)
	  years render_months in

    transcript_dom##innerHTML <- Js.string "";
    render_years () >|=
      List.iter (Dom.appendChild transcript_dom *< Html5.To_dom.of_element)
}}

let transcript_handler (room_jid, (tI, (tF, pat))) () =
  let open Html5.D in
  let transcript_div = div ~a:[a_class ["transcript"]] [] in
  let room_node = Node.of_string room_jid in
  lwt room =
    match_lwt Muc_room.stored_of_node room_node with
    | None -> Lwt.fail Eliom_common.Eliom_404
    | Some room -> Lwt.return room in
  let min_time = Option.get_else Unix.time (Muc_room.min_message_time room) in
  let relevant_message msg =
    let open Batyr_presence in
    Lwt.return begin
      if Resource.node (Message.sender msg) != room_node then None else
      Some {
	msg_time = Message.seen_time msg;
	msg_sender_cls = "jid";
	msg_sender = Resource.resource_name (Message.sender msg);
	msg_subject = Message.subject msg;
	msg_thread = Message.thread msg;
	msg_body = Message.body msg;
      }
    end in
  let update_events =
    Lwt_react.E.fmap_s relevant_message Batyr_presence.messages in
  let update_comet =
    Eliom_comet.Channel.create (Lwt_react.E.to_stream update_events) in
  let info_span = span ~a:[a_class ["error"]] [] in
  let clear_handler =
    {{fun ev ->
      let info_dom = Html5.To_dom.of_span %info_span in
      let search_dom =
	Domx_html.element_by_id Dom_html.CoerceTo.input "search_text" in
      info_dom##className <- Js.string "";
      info_dom##innerHTML <- Js.string "";
      search_dom##value <- Js.string "";
      let clear_dom =
	Domx_html.element_by_id Dom_html.CoerceTo.button "clear_search" in
      clear_dom##disabled <- Js._true;
      Lwt.ignore_result begin
	update_transcript_view ~room:%room_jid ~min_time:%min_time
			       (Html5.To_dom.of_div %transcript_div)
			       %update_comet
      end
    }} in
  let clear_button =
    button ~a:[a_onclick clear_handler; a_id "clear_search";
	       a_disabled `Disabled]
	   ~button_type:`Button [pcdata "all"] in
  let search_handler =
    {{fun ev ->
      let info_dom = Html5.To_dom.of_span %info_span in
      let search_dom =
	Domx_html.element_by_id Dom_html.CoerceTo.input "search_text" in
      let pat = match Js.to_string search_dom##value
		with "" -> None | s -> Some s in
      (Html5.To_dom.of_button %clear_button)##disabled <- Js.bool (pat = None);
      info_dom##className <- Js.string "";
      info_dom##innerHTML <- Js.string "";
      Lwt.ignore_result begin try_lwt
	update_transcript_view ~room:%room_jid ~min_time:%min_time ?pat
			       (Html5.To_dom.of_div %transcript_div)
			       %update_comet
      with Eliom_lib.Exception_on_server msg ->
	info_dom##className <- Js.string "error";
	info_dom##innerHTML <- Js.string msg;
	Lwt.return_unit
      end
    }} in
  let search_handler_mouse =
    {{fun ev -> %search_handler (ev :> Dom_html.event Js.t)}} in
  let search_button = button ~a:[a_onclick search_handler_mouse]
			     ~button_type:`Button [pcdata "matching"] in
  let search_input = input ~a:[a_id "search_text"; a_onchange search_handler]
			   ~input_type:`Text () in
  ignore {unit{
    let transcript_dom = Html5.To_dom.of_div %transcript_div in
    let tI = ref %tI in
    let tF = ref %tF in
    let pat = ref %pat in
    Lwt.ignore_result begin
      update_transcript_view ~room:%room_jid ~min_time:%min_time ?pat:!pat
			     transcript_dom %update_comet
    end
  }};
  let help_span =
    let bop s = span ~a:[a_class ["bnf-op"]] [pcdata s] in
    let op s = span [bop "'"; pcdata s; bop "'"] in
    let vn s = span ~a:[a_style "font-style: italic"] [pcdata s] in
    let sp = pcdata " " in
    span ~a:[a_class ["help"]] [
      vn "pat"; sp; bop "::="; sp;
      vn "word"; sp; bop "|"; sp;
      op "\""; vn "string"; op "\""; sp; bop "|"; sp;
      op "("; vn "pat"; op ")"; sp; bop "|"; sp;
      op "!"; vn "pat"; sp; bop "|"; sp;
      vn "pat"; sp; vn "pat"; sp; bop "|"; sp;
      vn "pat"; sp; op "|"; sp; vn "pat"
    ] in
  Lwt.return (Batyrweb_tools.D.page
    (sprintf "Transcript of %s" room_jid)
    [ div
      [ pcdata "Show "; clear_button;
	pcdata " or "; search_button; search_input; pcdata " ";
	info_span; pcdata " "; help_span ];
      transcript_div ]
  )

let () = Main_app.register ~service:transcript_service transcript_handler
