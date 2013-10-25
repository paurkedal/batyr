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

open Batyr_data
open Batyr_prereq
open Batyr_xmpp
open CalendarLib
open Printf
open Unprime
open Unprime_option

let section = Lwt_log.Section.make "batyr.presence"

module Message = struct
  type t = {
    seen_time : float;
    sender : Peer.t;
    recipient : Peer.t;
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

let chat_sessions = Hashtbl.create 11

let on_version ev jid_from jid_to lang () =
  match ev with
  | Chat.IQGet _ ->
    let el = Chat_version.(encode {name = "batyr"; version = "0.1";
                                   os = Sys.os_type}) in
    Lwt.return (Chat.IQResult (Some el))
  | Chat.IQSet _ -> Lwt.fail Chat.BadRequest

let string_of_message_type = function
  | Chat.Normal -> "normal"
  | Chat.Chat -> "chat"
  | Chat.Groupchat -> "groupchat"
  | Chat.Headline -> "headline"

let is_transcribed jid = true (* FIXME *)

let message_events, emit_message = Lwt_react.E.create ()

let on_message chat stanza =
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
	Lwt_log.ign_debug_f "Got delay stamp %s." delay_stamp;
	try
	  let fmt = if delay_legacy then "%Y%m%dT%T%z" else "%FT%TZ%z" in
	  let t =
	  Calendar.to_unixfloat
		(Printer.Calendar.from_fstring fmt (delay_stamp ^ "+0000")) in
	  Lwt_log.ign_debug_f "Converted to %f" t; t
	with Invalid_argument msg ->
	  Lwt_log.ign_error_f "Received invalid <delay/> stamp: %s" msg;
	  Unix.time () in
    let sender = Peer.of_jid sender in
    let recipient = Peer.of_jid (JID.of_string recipient) in
    let subject = stanza.Chat.content.Chat.subject in
    let thread = stanza.Chat.content.Chat.thread in
    let body = stanza.Chat.content.Chat.body in
    let msg = Message.make ~seen_time ~sender ~recipient ~message_type
			   ?subject ?thread ?body () in
    emit_message msg;
    if is_transcribed recipient then
      lwt sender_id = Peer.id sender in
      lwt recipient_id = Peer.id recipient in
      Batyr_db.(use begin fun dbh ->
	dbh#command
	  ~params:[|timestamp_of_epoch seen_time; or_null stanza.Chat.id;
		    string_of_int sender_id; string_of_int recipient_id;
		    string_of_message_type message_type;
		    or_null subject; or_null thread; or_null body|]
	  "INSERT INTO batyr.messages (seen_time, auxid, \
				       sender_id, recipient_id, \
				       message_type, subject, thread, body) \
	   VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
      end)
    else
      Lwt.return_unit
  | _ -> Lwt.return_unit

let on_message_error chat ?id ?jid_from ?jid_to ?lang error =
  let jid_from = Option.map JID.string_of_jid jid_from in
  let props = []
    |> Option.fold (fun s acc -> sprintf "recipient = %s" s :: acc) jid_to
    |> Option.fold (fun s acc -> sprintf "sender = %s" s :: acc) jid_from
    |> Option.fold (fun s acc -> sprintf "id = %s" s :: acc) id in
  Lwt_log.error_f "Message error; %s; %s"
		  (String.concat ", " props) error.StanzaError.err_text

let chat_handler account_id chat =
  Chat.register_iq_request_handler chat Chat_version.ns_version on_version;
  Chat.register_stanza_handler chat (Chat.ns_client, "message")
    (Chat.parse_message ~callback:on_message ~callback_error:on_message_error);
  Batyr_db.(use (fun dbh ->
    dbh#query_list Decode.(int ** option epoch)
      ~params:[|string_of_int account_id|]
      "SELECT peer_id, \
	      (SELECT max(seen_time) \
	       FROM batyr.messages JOIN batyr.peers AS sender \
		 ON sender_id = sender.peer_id \
	       WHERE sender.node_id = node_id) \
       FROM batyr.muc_presence NATURAL JOIN batyr.peers \
       WHERE account_id = $1 AND is_present = true")) >>=
    Lwt_list.iter_s
      (fun (peer_id, since) ->
	lwt peer = Peer.of_id peer_id in
	lwt seconds =
	  begin match since with
	  | None ->
	    Lwt_log.info_f ~section "Entering %s, no previous logs."
			   (Peer.to_string peer) >>
	    Lwt.return_none
	  | Some t ->
	    let t = Unix.time () -. t -. 1.0 in (* FIXME: Need <delay/> *)
	    Lwt_log.info_f ~section "Entering %s, seconds = %f"
			   (Peer.to_string peer) t >>
	    Lwt.return (Some (int_of_float t))
	  end in
	Chat_muc.enter_room ?seconds chat (Peer.jid peer))

let start_chat_sessions () =
  Batyr_db.use begin fun dbh ->
    dbh#query_list Batyr_db.Decode.(int ** int ** string)
      "SELECT peer_id, server_port, client_password \
       FROM batyr.accounts NATURAL JOIN batyr.peers \
       WHERE is_active = true" >>=
    Lwt_list.iter_s (fun (peer_id, (port, password)) ->
      Peer.of_id peer_id >|= fun peer ->
      let {JID.lnode; JID.ldomain; JID.lresource} = Peer.jid peer in
      let params = Chat_params.make ~server:ldomain ~port ~username:lnode
				    ~password ~resource:lresource () in
      let session_key = ldomain, port, lnode, lresource in
      if not (Hashtbl.mem chat_sessions session_key) then begin
	Hashtbl.add chat_sessions session_key true;
	Lwt.async (fun () -> with_chat (chat_handler peer_id) params)
      end)
  end
