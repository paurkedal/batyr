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
      {id = -1; domain_name; node_name; beacon = Batyr_cache.dummy_beacon}
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
    {id = -1; domain_name = ""; node_name = ""; beacon = Batyr_cache.dummy_beacon}

  let create ~domain_name ?(node_name = "") () =
    Data_cache.merge data_cache
      (Batyr_cache.cache Batyr_cache.Grade.basic
	(fun beacon -> {id = -1; domain_name; node_name; beacon}))

  let domain_name {domain_name} = domain_name
  let node_name {node_name} = node_name

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    if lresource <> "" then
      invalid_arg "Batyr_data.Node.of_jid: Non-empty resource.";
    create ~domain_name:ldomain ~node_name:lnode ()

  let jid {domain_name; node_name} = JID.make_jid node_name domain_name ""

  let to_string node_name = JID.string_of_jid (jid node_name)
  let of_string s = of_jid (JID.of_string s)

  let of_id id =
    try Lwt.return (Id_cache.find_key id_cache id)
    with Not_found ->
      Batyr_db.use
	(fun dbh ->
	  dbh#start_accounting;
	  dbh#query_single Batyr_db.Decode.(string ** string)
	    ~params:[|string_of_int id|]
	    "SELECT domain_name, node_name \
	     FROM batyr.nodes NATURAL JOIN batyr.domains \
	     WHERE node_id = $1" >|= fun (domain_name, node_name) ->
	  let grade = dbh#stop_accounting in
	  Batyr_cache.cache grade
	    (fun beacon -> {id; domain_name; node_name; beacon}))
	>|= fun node ->
      try Id_cache.find id_cache node
      with Not_found ->
	Data_cache.add data_cache node;
	Id_cache.add id_cache node;
	node

  let id node =
    if node.id >= 0 then Lwt.return node.id else
    Batyr_db.use
      (fun dbh ->
	dbh#start_accounting;
	dbh#query_single Batyr_db.Decode.int
	  ~params:[|node.domain_name; node.node_name|]
	  "SELECT batyr.make_node($1, $2)" >|= fun id ->
	node.id <- id;
	Id_cache.add id_cache node;
	id)
end

module Peer = struct
  type t = {
    mutable id : int;
    domain_name : string;
    node_name : string;
    resource : string;
    beacon : Batyr_cache.beacon;
  }

  module Data_bijection = struct
    type domain = t
    type codomain = string * string * string
    let f {domain_name; node_name; resource} =
      (domain_name, node_name, resource)
    let f_inv (domain_name, node_name, resource) = {
      id = -1; domain_name; node_name; resource;
      beacon = Batyr_cache.dummy_beacon;
    }
    let beacon {beacon} = beacon
  end

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {id} = assert (id >= 0); id
    let f_inv id = {
      id; domain_name = ""; node_name = ""; resource = "";
      beacon = Batyr_cache.dummy_beacon;
    }
    let beacon {beacon} = beacon
  end

  module Data_cache = Batyr_cache.Cache_of_bijection (Data_bijection)
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let data_cache = Data_cache.create 23
  let id_cache = Id_cache.create 23

  let create ~domain_name ?(node_name = "") ?(resource = "") () =
    Data_cache.merge data_cache
      (Batyr_cache.cache Batyr_cache.Grade.basic
	(fun beacon -> {id = -1; domain_name; node_name; resource; beacon}))

  let domain_name {domain_name} = domain_name
  let node_name {node_name} = node_name
  let node {domain_name; node_name} = Node.create ~domain_name ~node_name ()
  let resource {resource} = resource

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    create ~domain_name:ldomain ~node_name:lnode ~resource:lresource ()

  let jid {domain_name; node_name; resource} =
    JID.make_jid node_name domain_name resource

  let to_string p = JID.string_of_jid (jid p)
  let of_string s = of_jid (JID.of_string s)

  let of_id id =
    try Lwt.return (Id_cache.find_key id_cache id)
    with Not_found ->
      Batyr_db.use
	(fun dbh ->
	  dbh#start_accounting;
	  dbh#query_single Batyr_db.Decode.(string ** string ** string)
	    ~params:[|string_of_int id|]
	    "SELECT domain_name, node_name, resource \
	     FROM batyr.peers NATURAL JOIN batyr.nodes \
			      NATURAL JOIN batyr.domains \
	     WHERE peer_id = $1" >|= fun (domain_name, (node_name, resource)) ->
	  let grade = dbh#stop_accounting in
	  Batyr_cache.cache grade
	    (fun beacon -> {id; domain_name; node_name; resource; beacon}))
	>|= fun peer ->
      try Id_cache.find id_cache peer
      with Not_found ->
	Data_cache.add data_cache peer;
	Id_cache.add id_cache peer;
	peer

  let id peer =
    if peer.id >= 0 then Lwt.return peer.id else
    Batyr_db.use
      (fun dbh ->
	dbh#start_accounting;
	dbh#query_single Batyr_db.Decode.int
	  ~params:[|peer.domain_name; peer.node_name; peer.resource|]
	  "SELECT batyr.make_peer($1, $2, $3)" >|= fun id ->
	peer.id <- id;
	Id_cache.add id_cache peer;
	id)
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

  module Node_bijection = struct
    type domain = t
    type codomain = Node.t
    let f {node} = node
    let f_inv node = {
      node; alias = None; description = None; transcribe = false;
      min_message_time = None; beacon = Batyr_cache.dummy_beacon;
    }
    let beacon {beacon} = beacon
  end

  module Node_cache = Batyr_cache.Cache_of_bijection (Node_bijection)
  let node_cache = Node_cache.create 23

  let node {node} = node
  let alias {alias} = alias
  let description {description} = description
  let min_message_time {min_message_time} = min_message_time

  let of_node node =
    try Lwt.return (Node_cache.find_key node_cache node)
    with Not_found ->
      lwt node_id = Node.id node in
      Batyr_db.use
	(fun dbh ->
	  dbh#start_accounting;
	  dbh#query_single
	    Batyr_db.Decode.(option string ** option string ** bool **
			     option epoch)
	    ~params:[|string_of_int node_id|]
	    "SELECT room_alias, room_description, transcribe, \
		    (SELECT min(seen_time) \
		       FROM batyr.messages \
		       JOIN (batyr.peers NATURAL JOIN batyr.nodes) AS sender \
			 ON sender_id = sender.peer_id \
		      WHERE node_id = $1) \
	       FROM batyr.muc_rooms WHERE node_id = $1"
	    >|= fun (alias, (description, (transcribe, min_message_time))) ->
	  let grade = dbh#stop_accounting in
	  Batyr_cache.cache grade (fun beacon ->
	    {node; alias; description; transcribe; min_message_time; beacon}))
end
