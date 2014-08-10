(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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

open Batyr_data
open Batyr_prereq
open Batyr_xmpp
open CalendarLib
open Printf
open Unprime
open Unprime_list
open Unprime_option

let section = Lwt_log.Section.make "batyr.presence"

module Message = struct
  type t = {
    seen_time : float;
    sender : Resource.t;
    recipient : Resource.t;
    message_type : Chat.message_type;
    subject : string option;
    thread : string option;
    body : string option;
  }

  let seen_time {seen_time} = seen_time
  let make ~seen_time ~sender ~recipient
	   ~message_type ?subject ?thread ?body () =
    {seen_time; sender; recipient; message_type; subject; thread; body}
  let sender {sender} = sender
  let recipient {recipient} = recipient
  let message_type {message_type} = message_type
  let subject {subject} = subject
  let thread {thread} = thread
  let body {body} = body
end

type muc_session = {
  ms_room : Muc_room.t;
  ms_users_by_nick : (string, Muc_user.t) Hashtbl.t;
  ms_emit_presence : ?step: React.step ->
		     Chat.presence_content Chat.stanza -> unit;
  ms_presences : Chat.presence_content Chat.stanza React.E.t;
  ms_emit_message : ?step: React.step -> Message.t -> unit;
  ms_messages : Message.t React.E.t;
  ms_is_present : bool React.S.t;
}

type chat_session = {
  cs_chat : Chat.chat;
  cs_rooms : (int, muc_session) Hashtbl.t;
  cs_emit_presence : ?step: React.step ->
		     Chat.presence_content Chat.stanza -> unit;
  cs_presences : Chat.presence_content Chat.stanza React.E.t;
  cs_emit_message : ?step: React.step -> Message.t -> unit;
  cs_messages : Message.t React.E.t;
  mutable cs_retained_events : (unit -> unit) list;
}

let chat_sessions = Hashtbl.create 11

let string_of_message_type = function
  | Chat.Normal -> "normal"
  | Chat.Chat -> "chat"
  | Chat.Groupchat -> "groupchat"
  | Chat.Headline -> "headline"

let messages, emit_message = Lwt_react.E.create ()

let on_muc_message cs ms msg =
  let muc_author =
    let nick = Resource.resource_name (Message.sender msg) in
    try Muc_user.resource (Hashtbl.find ms.ms_users_by_nick nick)
    with Not_found -> None in
  if Muc_room.transcribe ms.ms_room then
    let author_id = Option.search Resource.cached_id muc_author in
    lwt sender_id = Resource.store (Message.sender msg) in
    lwt recipient_id = Resource.store (Message.recipient msg) in
    Batyr_db.(use begin fun dbh ->
      dbh#command
	~params:[|timestamp_of_epoch (Message.seen_time msg);
		  string_of_int sender_id;
		  or_null (Option.map string_of_int author_id);
		  string_of_int recipient_id;
		  string_of_message_type (Message.message_type msg);
		  or_null (Message.subject msg);
		  or_null (Message.thread msg);
		  or_null (Message.body msg)|]
	"INSERT INTO batyr.messages (seen_time, \
				     sender_id, author_id, recipient_id, \
				     message_type, subject, thread, body) \
	 VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
    end)
  else Lwt.return_unit

let on_message cs chat stanza =
  let open Xml in
  List.iter
    (function
      | Xmlelement ((None, tag), attrs, els) ->
	Lwt_log.ign_debug_f ~section "Unprocessed tag %s" tag
      | Xmlelement ((Some ns, tag), attrs, els) ->
	Lwt_log.ign_debug_f ~section "Unprocessed tag [%s]:%s" ns tag
      | Xmlcdata s ->
	Lwt_log.ign_debug_f ~section "Unprocessed cdata \"%s\""
			    (String.escaped s))
    Chat.(stanza.x);
  match Chat.(stanza.jid_from, stanza.jid_to, stanza.content.message_type) with
  | Some sender, Some recipient, Some message_type ->
    let seen_time =
      match Chat.(stanza.content.message_delay) with
      | None -> Unix.time ()
      | Some {Chat.delay_stamp; Chat.delay_legacy} ->
	Lwt_log.ign_debug_f ~section "Got delay stamp %s." delay_stamp;
	try
	  let fmt = if delay_legacy then "%Y%m%dT%T%z" else "%FT%TZ%z" in
	  let t =
	  Calendar.to_unixfloat
		(Printer.Calendar.from_fstring fmt (delay_stamp ^ "+0000")) in
	  Lwt_log.ign_debug_f ~section "Converted to %f" t; t
	with Invalid_argument msg ->
	  Lwt_log.ign_error_f ~section "Received invalid <delay/> stamp: %s"
			      msg;
	  Unix.time () in
    let sender = Resource.of_jid sender in
    let muc_room = Muc_room.cached_of_node (Resource.node sender) in
    let recipient = Resource.of_jid (JID.of_string recipient) in
    let subject = stanza.Chat.content.Chat.subject in
    let thread = stanza.Chat.content.Chat.thread in
    let body = stanza.Chat.content.Chat.body in
    if subject = None && thread = None && body = None then Lwt.return_unit else
    begin
      let msg = Message.make ~seen_time ~sender ~recipient ~message_type
			     ?subject ?thread ?body () in
      let step = React.Step.create () in	(* NB! No yield until ... *)
      cs.cs_emit_message ~step msg;
      emit_message ~step msg;
      match muc_room with
      | None ->
	React.Step.execute step;		(* ... here, and *)
	Lwt.return_unit
      | Some muc_room ->
	let room_id = Option.get (Node.cached_id (Muc_room.node muc_room)) in
	let ms = Hashtbl.find cs.cs_rooms room_id in
	ms.ms_emit_message ~step msg;
	React.Step.execute step;		(* ... here. *)
	on_muc_message cs ms msg
    end
  | _ -> Lwt.return_unit

let extract_muc_user nick =
  List.search
    begin function
    | Xml.Xmlelement ((ns_muc_user, "x"), _, _) as el ->
      let user = Chat_muc.User.decode el in
      begin match user.Chat_muc.User.item with
      | None -> None
      | Some item ->
	let open Chat_muc in
	let jid = item.User.jid in
	let role = Option.get_or RoleNone item.Chat_muc.User.role in
	let affiliation = Option.get_or AffiliationNone item.User.affiliation in
	Some (Muc_user.make ~nick ?jid ~role ~affiliation ())
      end
    | _ -> None
    end

let entered_room_by_node cs node =
  Option.search
    (fun id ->
      try Some (Hashtbl.find cs.cs_rooms id)
      with Not_found -> None)
    (Node.cached_id node)

let on_presence cs chat stanza =
  match stanza.Chat.jid_from with
  | None ->
    Lwt_log.warning ~section "Ignoring presence stanza lacking \"from\"."
  | Some sender_jid ->
    Lwt_log.debug_f ~section "Received %s from %s."
      (match Chat.(stanza.content.presence_type) with
       | Some pt -> Chat.string_of_presence_type pt
       | None -> "presence")
      (JID.string_of_jid sender_jid) >>
    begin match Chat.(stanza.content.presence_type) with
    | Some Chat.Subscribe ->
      Chat.send_presence cs.cs_chat ~jid_to:sender_jid ~kind:Chat.Subscribed ()
    | _ -> Lwt.return_unit
    end >>
    let entered_room_node = Node.of_jid (JID.bare_jid sender_jid) in
    let step = React.Step.create () in		(* NB! No yield until ... *)
    cs.cs_emit_presence ~step stanza;
    begin match entered_room_by_node cs entered_room_node with
    | None ->
      React.Step.execute step;			(* ... here, and *)
      Lwt.return_unit
    | Some ms ->
      ms.ms_emit_presence ~step stanza;
      React.Step.execute step;			(* ... here. *)
      let nick = sender_jid.JID.lresource in
      let user_opt = extract_muc_user nick stanza.Chat.x in
      begin match Chat.(stanza.content.presence_type), user_opt with
      | None, Some user ->
	Hashtbl.replace ms.ms_users_by_nick nick user;
	Lwt_log.debug_f ~section "User %s is available in %s."
			(Muc_user.to_string user)
			(Muc_room.to_string ms.ms_room)
      | Some Chat.Unavailable, Some user ->
	Hashtbl.remove ms.ms_users_by_nick nick;
	Lwt_log.debug_f ~section "User %s is unavailable in %s."
			(Muc_user.to_string user)
			(Muc_room.to_string ms.ms_room)
      | _ -> Lwt.return_unit
      end
    end

let on_error ~kind cs ?id ?jid_from ?jid_to ?lang error =
  let jid_from = Option.map JID.string_of_jid jid_from in
  let props = []
    |> Option.fold (fun s acc -> sprintf "recipient = %s" s :: acc) jid_to
    |> Option.fold (fun s acc -> sprintf "sender = %s" s :: acc) jid_from
    |> Option.fold (fun s acc -> sprintf "id = %s" s :: acc) id in
  Lwt_log.error_f ~section "%s error; %s; %s" kind
		  (String.concat ", " props) error.StanzaError.err_text

let retain_event cs ev =
  cs.cs_retained_events <- (fun () -> ev; ()) :: cs.cs_retained_events

let track_presence_of my_jid is_present stanza =
  match Chat.(stanza.jid_from, stanza.content.presence_type) with
  | Some sender_jid, Some Chat.Unavailable when sender_jid = my_jid ->
    begin match
      List.filter_map
	(fun e -> try Chat_muc.User.((Option.get ((decode e).item)).reason)
		  with Not_found -> None)
	Chat.(stanza.x)
    with
    | [] ->
      Lwt_log.notice_f ~section "I was logged out as %s for unknown reason."
		       (JID.string_of_jid my_jid) >>
      Lwt.return_false
    | reason :: _ ->
      Lwt_log.notice_f ~section "I was logged out as %s because: %s"
		       (JID.string_of_jid my_jid) reason >>
      Lwt.return_false
    end
  | Some sender_jid, None when sender_jid = my_jid ->
    Lwt_log.info_f ~section "I logged in as %s." (JID.string_of_jid my_jid) >>
    Lwt.return_true
  | _ ->
    Lwt.return is_present

let drive_signal
      ?(what = "reconnect")
      ?(fuzz = 0.1) ?(dt_edge = 1.0)
      ?(dt_min = 5.0) ?(dt_sat = 3600.0) ?(dt_avg = 86400.0)
      f s =
  let cond = Lwt_condition.create () in
  let driver () =
    let c_sat = dt_avg /. dt_sat in
    let lc_sat = log c_sat in
    let lc_min = log (dt_avg /. dt_min) in
    let t_norm = ref 0.0 in
    let c_cur = ref 0.0 in
    let next_delay () =
      let t = Unix.time () in
      let dt = t -. !t_norm in
      t_norm := t;
      c_cur := 1.0 +. !c_cur *. exp (-. dt /. dt_avg);
      let p = !c_cur /. c_sat in
      dt_avg *. exp ((p -. 1.0) *. lc_min -. p *. lc_sat) in
    while_lwt true do
      if React.S.value s
      then Lwt_condition.wait cond >> Lwt_unix.sleep dt_edge
      else
	let dt = next_delay () *. (1.0 +. fuzz *. (1.0 -. Random.float 2.0)) in
	f () >>
	Lwt_log.info_f ~section "Will wait %.3g s before next %s." dt what >>
	Lwt_unix.sleep dt
    done in
  Lwt.async driver;
  React.S.trace (fun v -> if not v then Lwt_condition.signal cond ()) s

let muc_handler cs (resource_id, (nick, since)) =
  let nick = Option.get_or (Chat.get_myjid cs.cs_chat).JID.node nick in
  lwt resource = Resource.stored_of_id resource_id in
  let room_node = Resource.node resource in
  let room_jid = Resource.jid resource in
  match_lwt Muc_room.stored_of_node room_node with
  | None ->
    Lwt_log.error_f ~section "Presence in non-room %s."
		    (Node.to_string room_node)
  | Some room ->
    (* FIXME: Need <delay/> *)
    let since_r = ref (Option.map ((+.) 0.05) since) in
    let muc_login () =
      lwt seconds =
	begin match !since_r with
	| None ->
	  Lwt_log.info_f ~section "Entering %s, no previous logs."
			 (Resource.to_string resource) >>
	  Lwt.return_none
	| Some t_disconn ->
	  let t = Unix.time () -. t_disconn in
	  Lwt_log.info_f ~section "Entering %s, seconds = %f"
			 (Resource.to_string resource) t >>
	  Lwt.return (Some (int_of_float t))
	end in
      Chat_muc.enter_room ?seconds ~nick cs.cs_chat room_jid in
    lwt room_id = Node.stored_id room_node >|= Option.get in
    let ms_presences, ms_emit_presence = React.E.create () in
    let ms_messages, ms_emit_message = React.E.create () in
    let my_jid = JID.replace_resource (Resource.jid resource) nick in
    let ms_is_present =
      Lwt_react.S.fold_s (track_presence_of my_jid) false ms_presences in
    let ms_is_present =
      drive_signal ~what:"MUC login" muc_login ms_is_present in
    let ms_is_present =
      React.S.trace (function false -> since_r := Some (Unix.time ()) | _ -> ())
		    ms_is_present in
    let ms = {
      ms_room = room;
      ms_users_by_nick = Hashtbl.create 17;
      ms_is_present;
      ms_presences; ms_emit_presence;
      ms_messages; ms_emit_message;
    } in
    Hashtbl.replace cs.cs_rooms room_id ms;
    Lwt.return_unit

let chat_handler session_key account_resource account_id chat =
  let cs_presences, cs_emit_presence = React.E.create () in
  let cs_messages, cs_emit_message = React.E.create () in
  let cs = {
    cs_chat = chat;
    cs_rooms = Hashtbl.create 23;
    cs_emit_presence; cs_presences;
    cs_emit_message;  cs_messages;
    cs_retained_events = [];
  } in
  Hashtbl.add chat_sessions session_key cs;
  Chat.register_stanza_handler chat (Chat.ns_client, "message")
    (Chat.parse_message ~callback:(on_message cs)
			~callback_error:(on_error ~kind:"message"));
  Chat.register_stanza_handler chat (Chat.ns_client, "presence")
    (Chat.parse_presence ~callback:(on_presence cs)
			 ~callback_error:(on_error ~kind:"presence"));
  Chat_version.register chat;
  Chat_ping.register chat;
  Chat_disco.register_info chat;
  Chat.send_presence chat ~jid_from:(Resource.jid account_resource)
		     ~show:Chat.ShowDND ~status:"logging" () >>
  Batyr_db.(use (fun dbh ->
    dbh#query_list Decode.(int ** option string ** option epoch)
      ~params:[|string_of_int account_id|]
      "SELECT resource_id, nick, \
	      (SELECT max(seen_time) \
	       FROM batyr.messages JOIN batyr.resources AS sender \
		 ON sender_id = sender.resource_id \
	       WHERE sender.node_id = node_id) \
       FROM batyr.muc_presence NATURAL JOIN batyr.resources \
       WHERE account_id = $1 AND is_present = true")) >>=
  Lwt_list.iter_s (muc_handler cs)

let start_chat_sessions () =
  Batyr_db.use (fun dbh ->
    dbh#query_list Batyr_db.Decode.(int ** int ** string)
      "SELECT resource_id, server_port, client_password \
       FROM batyr.accounts NATURAL JOIN batyr.resources \
       WHERE is_active = true") >>=
  Lwt_list.iter_s (fun (resource_id, (port, password)) ->
    Resource.stored_of_id resource_id >|= fun resource ->
    let {JID.lnode; JID.ldomain; JID.lresource} = Resource.jid resource in
    let params = Chat_params.make ~server:ldomain ~port ~username:lnode
				  ~password ~resource:lresource () in
    let session_key = ldomain, port, lnode, lresource in
    let reconnect_period = Batyr_config.presence_reconnect_period_cp#get in
    if not (Hashtbl.mem chat_sessions session_key) then begin
      let rec connect_loop () =
	let t_start = Unix.time () in
	begin try_lwt
	  with_chat (chat_handler session_key resource resource_id) params
	with
	| End_of_file ->
	  Lwt_log.error_f ~section "Lost connection."
	| xc ->
	  Lwt_log.error_f ~section "Caught %s." (Printexc.to_string xc) >>
	  Lwt_log.debug ~section (Printexc.get_backtrace ())
	end >>= fun () ->
	Hashtbl.remove chat_sessions session_key;
	let t_dur = Unix.time () -. t_start in
	let t_sleep = max 1.0 (reconnect_period -. t_dur) in
	Lwt_log.info_f ~section "Session lasted %g s, will re-connect in %g s."
		       t_dur t_sleep >>
	Lwt_unix.sleep t_sleep >> connect_loop () in
      Lwt.async connect_loop
    end)
