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
  open Unprime
  open Unprime_option
}}
{server{
  open Batyr_data
  open Batyr_prereq
  open Batyrweb_server
}}

(* Accounts *)
(* ======== *)

{shared{
  module Account_shared = struct
    type t = {
      account_jid : string;
      server_port : int;
      client_password : string;
      is_active : bool;
    } deriving (Json)

    let compare acA acB = compare acA.account_jid acB.account_jid
  end
}}
{client{
  module Account = struct
    include Account_shared

    type edit_dom = {
      ed_jid : Dom_html.inputElement Js.t;
      ed_password : Dom_html.inputElement Js.t;
      ed_server_port : Dom_html.inputElement Js.t;
      ed_is_active : Dom_html.inputElement Js.t;
    }

    let render_headers () =
      let open Html5.D in
      [ th [pcdata "JID"]; th [pcdata "Password"];
	th [pcdata "Server Port"]; th [pcdata "Active"] ]

    let render_row ac =
      let open Html5.D in
      [ td [pcdata ac.account_jid]; td [pcdata ac.client_password];
	td [pcdata (string_of_int ac.server_port)];
	td [pcdata (string_of_bool ac.is_active)] ]

    let render_edit_row ac_opt =
      let open Html5.D in
      let jid_input = input ~a:[a_size 12] ~input_type:`Text () in
      let password_input = input ~a:[a_size 12] ~input_type:`Text () in
      let server_port_input = input ~a:[a_size 4] ~input_type:`Text () in
      let is_active_input = input ~input_type:`Checkbox () in
      let ed = {
	ed_jid = Html5.To_dom.of_input jid_input;
	ed_password = Html5.To_dom.of_input password_input;
	ed_server_port = Html5.To_dom.of_input server_port_input;
	ed_is_active = Html5.To_dom.of_input is_active_input;
      } in
      Option.iter
	(fun ac ->
	  ed.ed_jid##value <- Js.string ac.account_jid;
	  ed.ed_password##value <- Js.string ac.client_password;
	  ed.ed_server_port##value <- Js.string (string_of_int ac.server_port);
	  ed.ed_is_active##checked <- Js.bool ac.is_active)
	ac_opt;
      let tds =
	[ td [jid_input];
	  td [password_input];
	  td [server_port_input];
	  td [is_active_input] ] in
      (ed, tds)

    let decode_row _ ed =
      { account_jid = Js.to_string ed.ed_jid##value;
	client_password = Js.to_string ed.ed_password##value;
	server_port = int_of_string (Js.to_string ed.ed_server_port##value);
	is_active = Js.to_bool ed.ed_is_active##checked; }
  end
}}
{server{
  module Account = struct
    include Account_shared

    let which_type = "account"

    let fetch_all () =
      lwt entries =
	Batyr_db.use begin fun dbh ->
	  dbh#query_list
	    Batyr_db.Decode.(int ** string ** int ** bool)
	    "SELECT resource_id, client_password, server_port, is_active \
	     FROM batyr.accounts"
	  end in
      let account_of_entry
	    (resource_id, (client_password, (server_port, is_active))) =
	lwt resource = Resource.stored_of_id resource_id in
	let account_jid = Resource.to_string resource in
	Lwt.return {account_jid; client_password; server_port; is_active} in
      Lwt_list.map_p account_of_entry entries

    let add old_account_opt account =
      lwt old_resource_id_opt =
	match old_account_opt with
	| None -> Lwt.return_none
	| Some old_account ->
	  lwt resource = Lwt.wrap1 Resource.of_string old_account.account_jid in
	  Resource.stored_id resource in
      lwt resource = Lwt.wrap1 Resource.of_string account.account_jid in
      lwt resource_id = Resource.store resource in
      let account = {account with account_jid = Resource.to_string resource} in
      let params = [|
	string_of_int resource_id;
	string_of_int account.server_port;
	account.client_password;
	string_of_bool account.is_active;
      |] in
      let sql =
	match old_resource_id_opt with
	| Some old_resource_id when old_resource_id = resource_id ->
	  "UPDATE batyr.accounts \
	   SET server_port = $2, client_password = $3, is_active = $4 \
	   WHERE resource_id = $1"
	| _ ->
	  "INSERT INTO batyr.accounts \
	      (resource_id, server_port, client_password, is_active) \
	   VALUES ($1, $2, $3, $4)" in
      Batyr_db.use begin fun dbh ->
	dbh#command ~params sql >>
	begin match old_resource_id_opt with
	| Some old_resource_id when old_resource_id <> resource_id ->
	  dbh#command ~params:[|string_of_int old_resource_id|]
	    "DELETE FROM batyr.accounts WHERE resource_id = $1"
	| _ -> Lwt.return_unit
	end
      end

    let remove account =
      lwt resource = Lwt.wrap1 Resource.of_string account.account_jid in
      lwt resource_id =
	match_lwt Resource.stored_id resource with
	| None -> Lwt.fail Eliom_common.Eliom_404
	| Some id -> Lwt.return id in
      Batyr_db.use begin fun dbh ->
	dbh#command
	  ~params:[|string_of_int resource_id|]
	  "DELETE FROM batyr.accounts WHERE resource_id = $1" >|= konst ()
      end
  end
}}
{shared{ module Accounts_editor = Bwl_table_editor.Make (Account) }}


(* Chatrooms *)
(* ========= *)

{shared{
  module Chatroom_shared = struct

    type t = {
      room_jid : string;
      room_alias : string option;
      room_description : string option;
      transcribe : bool;
    } deriving (Json)

    let compare r0 r1 = compare r0.room_jid r1.room_jid

  end
}}
{server{
  module Chatroom = struct
    include Chatroom_shared

    let which_type = "chat room"

    let fetch_all () =
      lwt entries =
	Batyr_db.use begin fun dbh ->
	  dbh#query_list
	    Batyr_db.Decode.(int ** option string ** option string ** bool)
	    "SELECT node_id, room_alias, room_description, transcribe \
	     FROM batyr.muc_rooms"
	  end in
      let chatroom_of_entry
	    (node_id, (room_alias, (room_description, transcribe))) =
	lwt node = Node.stored_of_id node_id in
	let room_jid = Node.to_string node in
	Lwt.return {room_jid; room_alias; room_description; transcribe} in
      Lwt_list.map_p chatroom_of_entry entries

    let add old_room_opt room =
      lwt old_node_id_opt =
	match old_room_opt with
	| None -> Lwt.return_none
	| Some old_room ->
	  lwt node = Lwt.wrap1 Node.of_string old_room.room_jid in
	  Node.stored_id node in
      lwt node = Lwt.wrap1 Node.of_string room.room_jid in
      lwt node_id = Node.store node in
      let room = {room with room_jid = Node.to_string node} in
      let params = [|
	string_of_int node_id;
	Batyr_db.or_null room.room_alias;
	Batyr_db.or_null room.room_description;
	string_of_bool room.transcribe;
      |] in
      let sql =
	match old_node_id_opt with
	| Some old_node_id when old_node_id = node_id ->
	  "UPDATE batyr.muc_rooms \
	   SET room_alias = $2, room_description = $3, transcribe = $4 \
	   WHERE node_id = $1"
	| _ ->
	  "INSERT INTO batyr.muc_rooms \
	      (node_id, room_alias, room_description, transcribe) \
	   VALUES ($1, $2, $3, $4)" in
      Batyr_db.use begin fun dbh ->
	dbh#command ~params sql >>
	match old_node_id_opt with
	| Some old_node_id when old_node_id <> node_id ->
	  dbh#command ~params:[|string_of_int old_node_id|]
	    "DELETE FROM batyr.muc_rooms WHERE node_id = $1"
	| _ -> Lwt.return_unit
      end

    let remove room =
      lwt node = Lwt.wrap1 Node.of_string room.room_jid in
      lwt node_id =
	match_lwt Node.stored_id node with
	| None -> Lwt.fail Eliom_common.Eliom_404
	| Some id -> Lwt.return id in
      Batyr_db.use begin fun dbh ->
	dbh#command
	  ~params:[|string_of_int node_id|]
	  "DELETE FROM batyr.muc_rooms WHERE node_id = $1"
      end
  end
}}
{client{
  let input_value_opt inp =
    match String.trim (Js.to_string inp##value) with
    | "" -> None
    | s -> Some s

  module Chatroom = struct
    include Chatroom_shared

    type edit_dom = {
      ed_jid : Dom_html.inputElement Js.t;
      ed_alias : Dom_html.inputElement Js.t;
      ed_description : Dom_html.inputElement Js.t;
      ed_transcribe : Dom_html.inputElement Js.t;
    }

    let render_headers () =
      let open Html5.D in
      [th [pcdata "JID"]; th [pcdata "Alias"]; th [pcdata "Description"];
       th [pcdata "Transcribe"]]

    let render_row r =
      Html5.D.([
	td [pcdata r.room_jid];
	td [pcdata (Option.get_or "-" r.room_alias)];
	td [pcdata (Option.get_or "-" r.room_description)];
	td [pcdata (string_of_bool r.transcribe)];
      ])

    let render_edit_row r_opt =
      let open Html5.D in
      let jid_inp = input ~a:[a_size 12] ~input_type:`Text () in
      let alias_inp = input ~a:[a_size 12] ~input_type:`Text () in
      let description_inp = input ~a:[a_size 12] ~input_type:`Text () in
      let transcribe_inp = input ~input_type:`Checkbox () in
      let ed = {
	ed_jid = Html5.To_dom.of_input jid_inp;
	ed_alias = Html5.To_dom.of_input alias_inp;
	ed_description = Html5.To_dom.of_input description_inp;
	ed_transcribe = Html5.To_dom.of_input transcribe_inp;
      } in
      Option.iter
	(fun r ->
	  ed.ed_jid##value <- Js.string r.room_jid;
	  ed.ed_alias##value <- Js.string (Option.get_or "" r.room_alias);
	  ed.ed_description##value <-
	    Js.string (Option.get_or "" r.room_description);
	  ed.ed_transcribe##checked <- Js.bool r.transcribe)
	r_opt;
      ed, [td [jid_inp]; td [alias_inp];
	   td [description_inp]; td [transcribe_inp]]

    let decode_row r_opt ed = {
      room_jid = Js.to_string ed.ed_jid##value;
      room_alias = input_value_opt ed.ed_alias;
      room_description = input_value_opt ed.ed_description;
      transcribe = Js.to_bool ed.ed_transcribe##checked;
    }
  end
}}
{shared{ module Chatrooms_editor = Bwl_table_editor.Make (Chatroom) }}


(* Main *)
(* ==== *)

module Admin_app = Eliom_registration.App
  (struct let application_name = "web-batyrweb_admin" end)

let admin_handler () () =

  let accounts_editor =
    Accounts_editor.create
      {Accounts_editor.clientside{Accounts_editor.clientside}}
      Accounts_editor.serverside in
  let chatrooms_editor =
    Chatrooms_editor.create
      {Chatrooms_editor.clientside{Chatrooms_editor.clientside}}
      Chatrooms_editor.serverside in

  Lwt.return Html5.D.(Batyrweb_tools.D.page "Administration" [
    h2 [pcdata "Accounts"];
    accounts_editor;
    h2 [pcdata "Chatrooms"];
    chatrooms_editor;
  ])

let () =
  Admin_app.register ~service:admin_service admin_handler
