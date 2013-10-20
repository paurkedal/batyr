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
    domain : string;
    node : string;
    beacon : Batyr_cache.beacon;
  }

  module Data_bijection = struct
    type domain = t
    type codomain = string * string
    let f {domain; node} = (domain, node)
    let f_inv (domain, node) =
      {id = -1; domain; node; beacon = Batyr_cache.dummy_beacon}
    let beacon {beacon} = beacon
  end

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {id} = assert (id >= 0); id
    let f_inv id =
      {id; domain = ""; node = ""; beacon = Batyr_cache.dummy_beacon}
    let beacon {beacon} = beacon
  end

  module Data_cache = Batyr_cache.Cache_of_bijection (Data_bijection)
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let data_cache = Data_cache.create 23
  let id_cache = Id_cache.create 23

  let create ~domain ?(node = "") () =
    Data_cache.merge data_cache
      (Batyr_cache.cache Batyr_cache.Grade.basic
	(fun beacon -> {id = -1; domain; node; beacon}))

  let domain {domain} = domain
  let node {node} = node

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    if lresource <> "" then
      invalid_arg "Batyr_data.Node.of_jid: Non-empty resource.";
    create ~domain:ldomain ~node:lnode ()

  let jid {domain; node} = JID.make_jid node domain ""

  let to_string node = JID.string_of_jid (jid node)
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
	     WHERE node_id = $1" >|= fun (domain, node) ->
	  let grade = dbh#stop_accounting in
	  Batyr_cache.cache grade (fun beacon -> {id; domain; node; beacon}))
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
	  ~params:[|node.domain; node.node|]
	  "SELECT batyr.make_node($1, $2)" >|= fun id ->
	node.id <- id;
	Id_cache.add id_cache node;
	id)
end

module Peer = struct
  type t = {
    mutable id : int;
    domain : string;
    node : string;
    resource : string;
    beacon : Batyr_cache.beacon;
  }

  module Data_bijection = struct
    type domain = t
    type codomain = string * string * string
    let f {domain; node; resource} = (domain, node, resource)
    let f_inv (domain, node, resource) = {
      id = -1; domain; node; resource;
      beacon = Batyr_cache.dummy_beacon;
    }
    let beacon {beacon} = beacon
  end

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {id} = assert (id >= 0); id
    let f_inv id = {
      id; domain = ""; node = ""; resource = "";
      beacon = Batyr_cache.dummy_beacon;
    }
    let beacon {beacon} = beacon
  end

  module Data_cache = Batyr_cache.Cache_of_bijection (Data_bijection)
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let data_cache = Data_cache.create 23
  let id_cache = Id_cache.create 23

  let create ~domain ?(node = "") ?(resource = "") () =
    Data_cache.merge data_cache
      (Batyr_cache.cache Batyr_cache.Grade.basic
	(fun beacon -> {id = -1; domain; node; resource; beacon}))

  let domain {domain} = domain
  let node {node} = node
  let resource {resource} = resource

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    create ~domain:ldomain ~node:lnode ~resource:lresource ()

  let jid {domain; node; resource} = JID.make_jid node domain resource

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
	     WHERE peer_id = $1" >|= fun (domain, (node, resource)) ->
	  let grade = dbh#stop_accounting in
	  Batyr_cache.cache grade
	    (fun beacon -> {id; domain; node; resource; beacon}))
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
	  ~params:[|peer.domain; peer.node; peer.resource|]
	  "SELECT batyr.make_peer($1, $2, $3)" >|= fun id ->
	peer.id <- id;
	Id_cache.add id_cache peer;
	id)
end
