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
  open Batyrweb_prereq
  open Eliom_content
  open Eliom_pervasives
  open Unprime
  open Unprime_list
  open Unprime_option
}}
{client{
  open Bwl_content
}}
{server{
  open Batyr_data
  open Batyr_prereq
  open Batyrweb_server
  open Caqti_lwt
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

    let of_account a =
      { account_jid = Resource.to_string (Account.resource a);
	client_password = Account.password a;
	server_port = Account.port a;
	is_active = Account.is_active a; }

    let fetch_all () = Batyr_data.Account.all () >|= List.map of_account

    let add old_account_opt account =
      lwt resource = Lwt.wrap1 Resource.of_string account.account_jid in
      let port = account.server_port in
      let password = account.client_password in
      let is_active = account.is_active in
      match old_account_opt with
      | None ->
	Batyr_data.Account.create ~resource ~port ~password ~is_active
				  () >|= ignore
      | Some old_account ->
	let old_resource = Resource.of_string old_account.account_jid in
	begin match_lwt Batyr_data.Account.of_resource old_resource with
	| None -> Lwt.return_unit
	| Some old_account' ->
	  Batyr_data.Account.update ~resource ~port ~password ~is_active
				    old_account'
	end

    let remove a =
      let resource = Resource.of_string a.account_jid in
      lwt resource_id = Resource.stored_id resource >|= Option.get in
      Batyr_data.Account.delete_id resource_id
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
      lwt entries = Batyr_db.use Batyr_sql.Admin.fetch_chatrooms in
      let chatroom_of_entry
	    (node_id, room_alias, room_description, transcribe) =
	lwt node = Node.stored_of_id node_id in
	let room_jid = Node.to_string node in
	Lwt.return {room_jid; room_alias; room_description; transcribe} in
      Lwt_list.rev_map_p chatroom_of_entry entries

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
      Batyr_db.use @@ fun conn ->
	Batyr_sql.Admin.upsert_chatroom (old_node_id_opt <> Some node_id)
	  node_id room.room_alias room.room_description room.transcribe conn >>
	match old_node_id_opt with
	| Some old_node_id when old_node_id <> node_id ->
	  Batyr_sql.Admin.delete_chatroom old_node_id conn
	| _ -> Lwt.return_unit

    let remove room =
      lwt node = Lwt.wrap1 Node.of_string room.room_jid in
      lwt node_id =
	match_lwt Node.stored_id node with
	| None -> Lwt.fail Eliom_common.Eliom_404
	| Some id -> Lwt.return id in
      Batyr_db.use (Batyr_sql.Admin.delete_chatroom node_id)
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


(* Presence *)
(* ======== *)

{shared{
  module Presence_shared = struct
    type t = {
      resource_jid : string;
      account_jid : string;
      is_present : bool;
      nick : string option;
    } deriving (Json)
    let compare presA presB = compare presA.resource_jid presB.resource_jid
  end
}}
{server{
  module Presence = struct
    include Presence_shared

    let which_type = "presence"

    let fetch_all () =
      Batyr_db.use Batyr_sql.Admin.fetch_presences >|=
	List.rev_map (fun (resource_jid, account_jid, is_present, nick) ->
			  {resource_jid; account_jid; is_present; nick})

    let add old_pres_opt pres =
      lwt old_resource_id_opt =
	match old_pres_opt with
	| None -> Lwt.return_none
	| Some old_pres ->
	  Resource.stored_id (Resource.of_string old_pres.resource_jid) in
      lwt resource_id = Resource.store (Resource.of_string pres.resource_jid) in
      lwt account_id =
	Resource.stored_id (Resource.of_string pres.account_jid)
	  >|= Option.get in
      Batyr_db.use @@ fun conn ->
	Batyr_sql.Admin.upsert_presence
	  (old_resource_id_opt <> Some resource_id)
	  resource_id account_id pres.nick pres.is_present conn >>
	match old_resource_id_opt with
	| Some old_resource_id when old_resource_id <> resource_id ->
	  Batyr_sql.Admin.delete_presence old_resource_id conn
	| _ -> Lwt.return_unit
    let remove pres =
      lwt resource_id =
	Resource.stored_id (Resource.of_string pres.resource_jid)
	  >|= Option.get in
      Batyr_db.use (Batyr_sql.Admin.delete_presence resource_id)
  end
}}
{client{
  open ReactiveData

  (* TODO: Utilize the ReactiveData API. *)
  let rlist_of_signal s =
    RList.make_from (React.S.value s) (React.S.diff (fun v _ -> RList.Set v) s)

  module Presence = struct
    include Presence_shared

    type edit_dom = {
      ed_resource_jid : Dom_html.inputElement Js.t;
      ed_account_jid : Dom_html.selectElement Js.t;
      ed_nick : Dom_html.inputElement Js.t;
      ed_is_present : Dom_html.inputElement Js.t;
    }

    let render_headers () =
      let open Html5.D in
      [th [pcdata "Resource"]; th [pcdata "Account"];
       th [pcdata "Nick"]; th [pcdata "Present"]]

    let render_row pres =
      let open Html5.D in
      [td [pcdata pres.resource_jid]; td [pcdata pres.account_jid];
       td [pcdata (Option.get_or "-" pres.nick)];
       td [pcdata (string_of_bool pres.is_present)]]

    let simple_select = Eliom_content_core.Html5.D.select
    let simple_option l = Eliom_content_core.Html5.D.(option (pcdata l))

    let dummy_opt = ([], "*", None, false)

    let account_optgroup : [`Option] Eliom_content.Html5.R.elt RList.t =
      let mkopt acct =
	Html5.F.option (Html5.F.pcdata acct.Account.account_jid) in
      rlist_of_signal @@
      React.S.map ~eq:(==)
	(fun accts ->
	  Accounts_editor.Enset.fold (List.push *< mkopt) accts []
	    |> List.rev)
	Accounts_editor.content

    let render_edit_row pres_opt =
      let open Html5.D in
      let inp_resource_jid = input ~a:[a_size 12] ~input_type:`Text () in
      let inp_account_jid = Html5.R.select account_optgroup in
      let inp_nick = input ~a:[a_size 8] ~input_type:`Text () in
      let inp_is_present = input ~input_type:`Checkbox () in
      let ed = {
	ed_resource_jid = Html5.To_dom.of_input inp_resource_jid;
	ed_account_jid = Html5.To_dom.of_select inp_account_jid;
	ed_nick = Html5.To_dom.of_input inp_nick;
	ed_is_present = Html5.To_dom.of_input inp_is_present;
      } in
      Option.iter
	(fun pres ->
	  ed.ed_resource_jid##value <- Js.string pres.resource_jid;
	  ed.ed_account_jid##value <- Js.string pres.account_jid;
	  ed.ed_nick##value <- Js.string (Option.get_or "" pres.nick);
	  ed.ed_is_present##checked <- Js.bool pres.is_present)
	pres_opt;
      ed, [td [inp_resource_jid]; td [inp_account_jid];
	   td [inp_nick]; td [inp_is_present]]

    let decode_row pres_opt ed =
      let resource_jid = Js.to_string ed.ed_resource_jid##value in
      let account_jid = Js.to_string ed.ed_account_jid##value in
      let nick = input_value_opt ed.ed_nick in
      let is_present = Js.to_bool ed.ed_is_present##checked in
      {resource_jid; account_jid; nick; is_present}
  end
}}
{shared{ module Presence_editor = Bwl_table_editor.Make (Presence) }}


(* Main *)
(* ==== *)

module Admin_app = Eliom_registration.App
  (struct let application_name = "admin" end)

let admin_handler () () =

  let accounts_editor = Html5.D.div [] in
  ignore {unit{
    Accounts_editor.clientside %accounts_editor %Accounts_editor.serverside
  }};

  let chatrooms_editor = Html5.D.div [] in
  ignore {unit{
    Chatrooms_editor.clientside %chatrooms_editor %Chatrooms_editor.serverside
  }};

  let presence_editor = Html5.D.div [] in
  ignore {unit{
    Presence_editor.clientside %presence_editor %Presence_editor.serverside
  }};

  Lwt.return Html5.D.(Batyrweb_tools.D.page "Administration" [
    h2 [pcdata "Accounts"];
    accounts_editor;
    h2 [pcdata "Presence"];
    presence_editor;
    h2 [pcdata "Chatrooms"];
    chatrooms_editor;
  ])

let () =
  Admin_app.register ~service:admin_service admin_handler
