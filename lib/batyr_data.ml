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

open Batyr_prereq
open Batyr_xmpp
open Unprime_option

let id_unknown = -1
let id_missing = -2

module Node = struct
  type t = {
    mutable id : int;
    domain_name : string;
    node_name : string;
    beacon : Batyr_cache.beacon;
  }

  module Data_bijection = struct
    type domain = t
    type codomain = string * string
    let f {domain_name; node_name} = (domain_name, node_name)
    let f_inv (domain_name, node_name) =
      {id = id_unknown; domain_name; node_name;
       beacon = Batyr_cache.dummy_beacon}
    let beacon {beacon} = beacon
  end

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {id} = assert (id >= 0); id
    let f_inv id =
      {id; domain_name = ""; node_name = ""; beacon = Batyr_cache.dummy_beacon}
    let beacon {beacon} = beacon
  end

  module Data_cache = Batyr_cache.Cache_of_bijection (Data_bijection)
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let data_cache = Data_cache.create 23
  let id_cache = Id_cache.create 23

  let dummy =
    {id = id_unknown; domain_name = ""; node_name = "";
     beacon = Batyr_cache.dummy_beacon}

  let create ~domain_name ?(node_name = "") () =
    Data_cache.merge data_cache
      (Batyr_cache.cache Batyr_cache.Grade.basic
	(fun beacon -> {id = id_unknown; domain_name; node_name; beacon}))

  let domain_name {domain_name} = domain_name
  let node_name {node_name} = node_name

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    if lresource <> "" then
      invalid_arg "Batyr_data.Node.of_jid: Non-empty resource.";
    create ~domain_name:ldomain ~node_name:lnode ()

  let jid {domain_name; node_name} = JID.make_jid node_name domain_name ""

  let to_string node_name = JID.string_of_jid (jid node_name)
  let of_string s = of_jid (JID.of_string s)

  let equal = (==)
  let hash {domain_name; node_name} = Hashtbl.hash (domain_name, node_name)

  let cached_of_id id =
    try Some (Id_cache.find_key id_cache id) with Not_found -> None
  let cached_id {id} = if id >= 0 then Some id else None

  let stored_of_id id =
    try Lwt.return (Id_cache.find_key id_cache id)
    with Not_found ->
      Batyr_db.use_accounted (Batyr_sql.Node.get id)
	>|= fun (cost, (domain_name, node_name)) ->
      let node =
	Batyr_cache.cache cost
	  (fun beacon -> {id; domain_name; node_name; beacon}) in
      try Id_cache.find id_cache node
      with Not_found ->
	Data_cache.add data_cache node;
	Id_cache.add id_cache node;
	node

  let stored_id node =
    if node.id >= 0 then Lwt.return (Some node.id) else
    if node.id = id_missing then Lwt.return_none else
    Batyr_db.use_accounted
      (Batyr_sql.Node.locate node.domain_name node.node_name)
      >|= fun (cost, id_opt) ->
    Batyr_cache.enrich cost node.beacon;
    match id_opt with
    | None -> node.id <- id_missing; None
    | Some id -> node.id <- id; Id_cache.add id_cache node; Some id

  let store node =
    if node.id >= 0 then Lwt.return node.id else
    Batyr_db.use_accounted
      (Batyr_sql.Node.store node.domain_name node.node_name)
      >|= fun (cost, id) ->
    Batyr_cache.enrich cost node.beacon;
    node.id <- id;
    Id_cache.add id_cache node;
    id
end

module Resource = struct
  type t = {
    mutable id : int;
    domain_name : string;
    node_name : string;
    resource_name : string;
    beacon : Batyr_cache.beacon;
  }

  module Data_bijection = struct
    type domain = t
    type codomain = string * string * string
    let f {domain_name; node_name; resource_name} =
      (domain_name, node_name, resource_name)
    let f_inv (domain_name, node_name, resource_name) = {
      id = id_unknown; domain_name; node_name; resource_name;
      beacon = Batyr_cache.dummy_beacon;
    }
    let beacon {beacon} = beacon
  end

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {id} = assert (id >= 0); id
    let f_inv id = {
      id; domain_name = ""; node_name = ""; resource_name = "";
      beacon = Batyr_cache.dummy_beacon;
    }
    let beacon {beacon} = beacon
  end

  module Data_cache = Batyr_cache.Cache_of_bijection (Data_bijection)
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let data_cache = Data_cache.create 23
  let id_cache = Id_cache.create 23

  let create ~domain_name ?(node_name = "") ?(resource_name = "") () =
    Data_cache.merge data_cache
      (Batyr_cache.cache Batyr_cache.Grade.basic
	(fun beacon ->
	 {id = id_unknown; domain_name; node_name; resource_name; beacon}))

  let domain_name {domain_name} = domain_name
  let node_name {node_name} = node_name
  let resource_name {resource_name} = resource_name
  let node {domain_name; node_name} = Node.create ~domain_name ~node_name ()

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    create ~domain_name:ldomain ~node_name:lnode ~resource_name:lresource ()

  let jid {domain_name; node_name; resource_name} =
    JID.make_jid node_name domain_name resource_name

  let to_string p = JID.string_of_jid (jid p)
  let of_string s = of_jid (JID.of_string s)

  let equal = (==)
  let hash {domain_name; node_name; resource_name} =
    Hashtbl.hash (domain_name, node_name, resource_name)

  let cached_of_id id =
    try Some (Id_cache.find_key id_cache id) with Not_found -> None
  let cached_id {id} = if id >= 0 then Some id else None

  let stored_of_id id =
    try Lwt.return (Id_cache.find_key id_cache id)
    with Not_found ->
      Batyr_db.use_accounted (Batyr_sql.Resource.get id)
	>|= fun (cost, (domain_name, node_name, resource_name)) ->
      let resource =
	Batyr_cache.cache cost
	  (fun beacon -> {id; domain_name; node_name; resource_name; beacon}) in
      try Id_cache.find id_cache resource
      with Not_found ->
	Data_cache.add data_cache resource;
	Id_cache.add id_cache resource;
	resource

  let stored_id resource =
    if resource.id >= 0 then Lwt.return (Some resource.id) else
    if resource.id = id_missing then Lwt.return_none else
    Batyr_db.use_accounted
      (Batyr_sql.Resource.locate resource.domain_name resource.node_name
				 resource.resource_name)
      >|= fun (cost, id_opt) ->
    Batyr_cache.enrich cost resource.beacon;
    match id_opt with
    | None -> resource.id <- id_missing; None
    | Some id -> resource.id <- id; Id_cache.add id_cache resource; Some id

  let store resource =
    if resource.id >= 0 then Lwt.return resource.id else
    Batyr_db.use_accounted
      (Batyr_sql.Resource.store resource.domain_name resource.node_name
				resource.resource_name)
      >|= fun (cost, id) ->
    Batyr_cache.enrich cost resource.beacon;
    resource.id <- id;
    Id_cache.add id_cache resource;
    id
end

module Muc_user = struct
  type t = {
    nick : string;
    resource : Resource.t option;
    role : Chat_muc.role;
    affiliation : Chat_muc.affiliation
  }
  let make ~nick ?jid ~role ~affiliation () =
    let resource = Option.map Resource.of_jid jid in
    {nick; resource; role; affiliation}
  let nick {nick} = nick
  let jid {resource} = Option.map Resource.jid resource
  let resource {resource} = resource
  let role {role} = role
  let affiliation {affiliation} = affiliation
  let to_string = function
    | {nick; resource = None} -> nick
    | {nick; resource = Some resource} ->
      nick ^ " <" ^ Resource.to_string resource ^ ">"
end

module Muc_room = struct
  type t = {
    node : Node.t;
    alias : string option;
    description : string option;
    transcribe : bool;
    min_message_time : float option;
    beacon : Batyr_cache.beacon;
  }
  type t' = t

  module Node_hashable = struct
    type t = t'
    let equal roomA roomB = Node.equal roomA.node roomB.node
    let hash {node} = Node.hash node
    let beacon {beacon} = beacon
  end

  module Node_cache = Batyr_cache.Cache_of_hashable (Node_hashable)
  let node_cache = Node_cache.create 23

  let node {node} = node
  let alias {alias} = alias
  let description {description} = description
  let transcribe {transcribe} = transcribe
  let min_message_time {min_message_time} = min_message_time
  let to_string {node} = Node.to_string node

  let make_dummy node = {
    node; alias = None; description = None; transcribe = false;
    min_message_time = None;
    beacon = Batyr_cache.dummy_beacon;
  }

  let cached_of_node node =
    try Some (Node_cache.find node_cache (make_dummy node))
    with Not_found -> None

  let stored_of_node node =
    try Lwt.return (Some (Node_cache.find node_cache (make_dummy node)))
    with Not_found ->
      begin match_lwt Node.stored_id node with
      | None -> Lwt.return_none
      | Some node_id ->
	Batyr_db.use_accounted (Batyr_sql.Muc_room.stored_of_node node_id)
	  >|= fun (cost, qr) ->
	Option.map
	  (fun (alias, description, transcribe, mmt) ->
	    let min_message_time =
	      Option.map CalendarLib.Calendar.to_unixfloat mmt in
	    let room =
	      Batyr_cache.cache cost (fun beacon ->
		{node; alias; description; transcribe; min_message_time;
		 beacon}) in
	    Node_cache.merge node_cache room)
	  qr
      end
end
