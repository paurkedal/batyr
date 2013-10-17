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

open Batyr_prereq
open Batyr_xmpp
open Printf
open Unprime
open Unprime_option

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

let on_message chat stanza =
  match Chat.(stanza.jid_from, stanza.jid_to, stanza.content.message_type) with
  | Some sender, Some recipient, Some message_type
      when is_transcribed recipient ->
    Batyr_db.(use begin fun dbh ->
      dbh#command
	~params:[|or_null stanza.Chat.id;
		  JID.string_of_jid sender; recipient;
		  string_of_message_type message_type;
		  or_null stanza.Chat.content.Chat.subject;
		  or_null stanza.Chat.content.Chat.thread;
		  or_null stanza.Chat.content.Chat.body|]
	"INSERT INTO batyr.messages (auxid, sender_id, recipient_id, \
				     message_type, subject, thread, body) \
	 VALUES ($1, batyr.make_jid($2, false), batyr.make_jid($3, false), \
		 $4, $5, $6, $7)"
    end)
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
    dbh#query_list Decode.string
      ~params:[|string_of_int account_id|]
      "SELECT jid FROM batyr.chatrooms NATURAL JOIN batyr.peers \
       WHERE account_id = $1 AND is_joined = true")) >>=
    Lwt_list.iter_s (fun jid -> Chat_muc.enter_room chat (JID.of_string jid))

let start_chat_sessions () =
  Batyr_db.use begin fun dbh ->
    dbh#query_array Batyr_db.Decode.(int ** string ** int ** string)
      "SELECT peer_id, jid, server_port, client_password \
       FROM batyr.accounts NATURAL JOIN batyr.peers \
       WHERE is_active = true" >|=
    Array.iter (fun (account_id, (jid_s, (port, password))) ->
      let {JID.node; JID.domain; JID.resource} = JID.of_string jid_s in
      let params = Chat_params.make ~server:domain ~port ~username:node
				    ~password ~resource () in
      let session_key = domain, port, node, resource in
      if not (Hashtbl.mem chat_sessions session_key) then begin
	Hashtbl.add chat_sessions session_key true;
	Lwt.async (fun () -> with_chat (chat_handler account_id) params)
      end)
  end
