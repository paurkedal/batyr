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
      room_jid : string;
      room_alias : string option;
      room_description : string option;
      transcribe : bool;
    } deriving (Json)

    let compare r0 r1 = compare r0.room_jid r1.room_jid
  end

  type chatroom = Chatroom_base.t deriving (Json)

  type update_insn =
    | Add_chatroom of chatroom
    | Remove_chatroom of chatroom
    deriving (Json)
}}

{server{
  open Batyr_data
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
		Batyr_db.Decode.(int ** option string ** option string ** bool)
		"SELECT node_id, room_alias, room_description, transcribe \
		 FROM batyr.muc_rooms NATURAL JOIN batyr.nodes"
	      end in
	  let chatroom_of_entry
		(node_id, (room_alias, (room_description, transcribe))) =
	    lwt node = Node.of_id node_id in
	    let room_jid = Node.to_string node in
	    Lwt.return {room_jid; room_alias; room_description; transcribe} in
	  Lwt_list.map_p chatroom_of_entry entries)
	(fun xc ->
	  Lwt_log.error_f "Failed to query connections: %s"
			  (Printexc.to_string xc) >>
	  Lwt.return [])

    let add room =
      Lwt.catch
	(fun () ->
	  lwt node = Lwt.wrap1 Node.of_string room.room_jid in
	  lwt node_id = Node.id node in
	  let room = {room with room_jid = Node.to_string node} in
	  Batyr_db.use begin fun dbh ->
	    dbh#query_single Batyr_db.Decode.bool
	      ~params:[|
		string_of_int node_id;
		Batyr_db.or_null room.room_alias;
		Batyr_db.or_null room.room_description;
		string_of_bool room.transcribe;
	      |]
	      "SELECT batyr.update_muc_room($1, $2, $3, $4)" >>= fun _ ->
	    (update (Some (Add_chatroom room)); Lwt.return (Ok ()))
	  end)
	(fun xc ->
	  Lwt_log.error_f "Failed to create or update %s: %s"
			  room.room_jid (Printexc.to_string xc) >>
	  Lwt.return (Failed "Error creating or updating room."))

    let remove room =
      Lwt.catch
	(fun () ->
	  lwt node = Lwt.wrap1 Node.of_string room.room_jid in
	  lwt node_id = Node.id node in
	  Batyr_db.use begin fun dbh ->
	    dbh#command
	      ~params:[|string_of_int node_id|]
	      "DELETE FROM batyr.muc_rooms WHERE node_id = $1"
	  end >|= fun () ->
	  update (Some (Remove_chatroom room)))
	(fun xc ->
	  Lwt_log.error_f "Failed to delete %s: %s"
			  room.room_jid (Printexc.to_string xc))
  end

  let sf_get_all = server_function Json.t<unit> Chatroom.get_all
  let sf_add = server_function Json.t<chatroom> Chatroom.add
  let sf_remove = server_function Json.t<chatroom> Chatroom.remove

}}

{client{
  open Batyrweb_client

  let input_value_opt inp =
    match String.trim (Js.to_string inp##value) with
    | "" -> None
    | s -> Some s

  module Chatroom = struct
    include Chatroom_base

    let render_row r =
      let remove_handler ev =
	Lwt.async (fun () -> %sf_remove r) in
      let remove_button =
	Html5.D.(button ~a:[a_onclick remove_handler] ~button_type:`Button
			[pcdata "delete"]) in
      Html5.D.([
	td [pcdata r.room_jid];
	td [pcdata (Option.get_or "-" r.room_alias)];
	td [pcdata (Option.get_or "-" r.room_description)];
	td [pcdata (string_of_bool r.transcribe)];
	td [remove_button];
      ])
  end

  module Chatrooms_live = Live_table (Chatroom)
}}

module Admin_app = Eliom_registration.App
  (struct let application_name = "web-batyrweb_admin" end)

let admin_handler () () =
  let error_td = Html5.D.(td ~a:[a_class ["error"]] []) in
  let jid_inp = Html5.D.(input ~input_type:`Text ()) in
  let alias_inp = Html5.D.(input ~input_type:`Text ()) in
  let description_inp = Html5.D.(input ~input_type:`Text ()) in
  let add_handler =
    {{fun ev ->
      let jid_dom = Html5.To_dom.of_input %jid_inp in
      let alias_dom = Html5.To_dom.of_input %alias_inp in
      let description_dom = Html5.To_dom.of_input %description_inp in
      let error_dom = Html5.To_dom.of_td %error_td in
      Lwt.async begin fun () ->
	%sf_add Chatroom.({
	  room_jid = Js.to_string jid_dom##value;
	  room_alias = input_value_opt alias_dom;
	  room_description = input_value_opt description_dom;
	  transcribe = false;
	}) >|=
	function
	| Ok () -> error_dom##innerHTML <- Js.string ""
	| Failed msg -> error_dom##innerHTML <- Js.string msg
      end
    }} in
  let add_button =
    Html5.D.(button ~a:[a_onclick add_handler] ~button_type:`Button
		    [pcdata "add"]) in
  let chatrooms_table =
    Html5.D.(table ~a:[a_class ["edit"]]
      (tr [th [pcdata "JID"]; th [pcdata "Alias"]; th [pcdata "Description"];
	   th [pcdata "Transcribe"]])
      [tr [td [jid_inp]; td [alias_inp]; td [description_inp];
	   td [add_button]; error_td]]
    ) in

  ignore {unit{
    let chatrooms_live =
      Chatrooms_live.create (Html5.To_dom.of_table %chatrooms_table) in

    Lwt.ignore_result
      (%sf_get_all () >|= List.iter (Chatrooms_live.add chatrooms_live));

    let update = function
      | Add_chatroom chatroom ->
	Chatrooms_live.add chatrooms_live chatroom
      | Remove_chatroom chatroom ->
	Chatrooms_live.remove chatrooms_live chatroom in
    Lwt.async (fun () -> Lwt_stream.iter update %update_comet)
  }};

  Lwt.return Html5.D.(Batyrweb_tools.D.page "Administration" [
    h2 [pcdata "Chatrooms"];
    chatrooms_table;
  ])

let () =
  Admin_app.register ~service:admin_service admin_handler
