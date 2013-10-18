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
  open Batyrweb_prereq
  open Eliom_content
  open Eliom_pervasives
  open Unprime_option

  module Chatroom_base = struct

    type t = {
      name : string;
      jid : string;
      transcribe : bool;
    } deriving (Json)

    let compare r0 r1 =
      let c = compare r0.name r1.name in
      if c <> 0 then c else compare r0.jid r1.jid
  end

  type chatroom = Chatroom_base.t deriving (Json)

  type update_insn =
    | Add_chatroom of chatroom
    | Remove_chatroom of chatroom
    deriving (Json)
}}

{server{
  open Batyr_prereq
  open Batyr_xmpp
  open Batyrweb_server
  open Printf

  let update_stream, update = Lwt_stream.create ()
  let update_comet : update_insn Eliom_comet.Channel.t =
    Eliom_comet.Channel.create ~scope:Eliom_common.site_scope update_stream

  module Chatroom = struct
    include Chatroom_base

    let get_all () =
      Lwt.catch
	(fun () ->
	  lwt entries =
	    Batyr_db.use begin fun dbh ->
	      dbh#query_list
		Batyr_db.Decode.(string ** string ** bool)
		"SELECT peerbin_name, jid, transcribe \
		 FROM batyr.peerbins NATURAL JOIN batyr.peers \
		 WHERE peerbin_type = 'chatroom'"
	      end in
	  let chatroom_of_entry (name, (jid, transcribe)) =
	    {name; jid; transcribe} in
	  Lwt.return (List.map chatroom_of_entry entries))
	(fun xc ->
	  Lwt_log.error_f "Failed to query connections: %s"
			  (Printexc.to_string xc) >>
	  Lwt.return [])

    let normalize_jid jid = JID.string_of_jid (JID.of_string jid)

    let add chatroom =
      if chatroom.name = ""
	then Lwt.return (Failed "Room name not specified.") else
      if try ignore (JID.of_string chatroom.jid); false with _ -> true
	then Lwt.return (Failed "Invalid Jabber id.") else
      let chatroom = {chatroom with jid = normalize_jid chatroom.jid} in
      Lwt.catch
	(fun () ->
	  Batyr_db.use begin fun dbh ->
	    match_lwt
	      dbh#query_option Batyr_db.Decode.(string)
		~params:[|chatroom.jid|]
		"SELECT peerbin_name \
		 FROM batyr.peerbins NATURAL JOIN batyr.peers \
		 WHERE jid = $1"
	    with
	    | None ->
	      dbh#query_single Batyr_db.Decode.unit
		~params:[|chatroom.name; chatroom.jid|]
		"SELECT batyr.connect('chatroom', $1, $2)" >>
	      (update (Some (Add_chatroom chatroom)); Lwt.return (Ok ()))
	    | Some room ->
	      Lwt.return (Failed (sprintf "Disconnect %s from %s first."
					  chatroom.jid room))
	  end)
	(fun xc ->
	  Lwt_log.error_f "Failed to connect %s to %s: %s"
			  chatroom.jid chatroom.name (Printexc.to_string xc) >>
	  Lwt.return (Failed "Server side error."))

    let remove chatroom =
      Lwt.catch
	(fun () ->
	  Batyr_db.use begin fun dbh ->
	    dbh#command
	      ~params:[|chatroom.jid|]
	      "UPDATE batyr.peers SET peerbin_id = NULL WHERE jid = $1"
	  end >|= fun () ->
	  update (Some (Remove_chatroom chatroom)))
	(fun xc ->
	  Lwt_log.error_f "Failed to disconnect %s from %s: %s"
			  chatroom.jid chatroom.name (Printexc.to_string xc))
  end

  let sf_connections = server_function Json.t<unit> Chatroom.get_all
  let sf_connect = server_function Json.t<chatroom> Chatroom.add
  let sf_disconnect = server_function Json.t<chatroom> Chatroom.remove

}}

{client{
  open Batyrweb_client

  module Chatroom = struct
    include Chatroom_base

    let render_row r =
      let disconnect_handler ev =
	Lwt.async (fun () -> %sf_disconnect r) in
      let disconnect_button =
	Html5.D.(button ~a:[a_onclick disconnect_handler] ~button_type:`Button
			[pcdata "< >"]) in
      Html5.D.([
	td [pcdata r.name];
	td [disconnect_button];
	td [pcdata r.jid];
	td [pcdata (string_of_bool r.transcribe)];
      ])
  end

  module Chatrooms_live = Live_table (Chatroom)
}}

module Admin_app = Eliom_registration.App
  (struct let application_name = "web-batyrweb_admin" end)

let admin_handler () () =
  let error_td = Html5.D.(td ~a:[a_class ["error"]] []) in
  let name_inp = Html5.D.(input ~input_type:`Text ()) in
  let jid_inp = Html5.D.(input ~input_type:`Text ()) in
  let connect_handler =
    {{fun ev ->
      let name_dom = Html5.To_dom.of_input %name_inp in
      let jid_dom = Html5.To_dom.of_input %jid_inp in
      let error_dom = Html5.To_dom.of_td %error_td in
      Lwt.async begin fun () ->
	%sf_connect Chatroom.({
	  name = Js.to_string name_dom##value;
	  jid = Js.to_string jid_dom##value;
	  transcribe = false;
	}) >|=
	function
	| Ok () -> error_dom##innerHTML <- Js.string ""
	| Failed msg -> error_dom##innerHTML <- Js.string msg
      end
    }} in
  let connect_button =
    Html5.D.(button ~a:[a_onclick connect_handler] ~button_type:`Button
		    [pcdata ">-<"]) in
  let chatrooms_table =
    Html5.D.(table ~a:[a_class ["edit"]]
      (tr [th [pcdata "Room"]; th []; th [pcdata "Jabber id"];
	   th [pcdata "Transcribe"]])
      [tr [td [name_inp]; td [connect_button]; td [jid_inp]; td []; error_td]]
    ) in

  ignore {unit{
    let chatrooms_live =
      Chatrooms_live.create (Html5.To_dom.of_table %chatrooms_table) in

    Lwt.ignore_result
      (%sf_connections () >|= List.iter (Chatrooms_live.add chatrooms_live));

    let update = function
      | Add_chatroom chatroom ->
	Chatrooms_live.add chatrooms_live chatroom
      | Remove_chatroom chatroom ->
	Chatrooms_live.remove chatrooms_live chatroom in
    Lwt.async (fun () -> Lwt_stream.iter update %update_comet)
  }};

  Lwt.return Html5.D.(Layout.D.page "Administration" [
    h2 [pcdata "Chatrooms"];
    chatrooms_table;
  ])

let () =
  Admin_app.register ~service:admin_service admin_handler
