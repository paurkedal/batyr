(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Batyr_cache
open Batyr_prereq
open Lwt.Infix
open Unprime_option

let id_unknown = -1
let id_missing = -2

module Node = struct
  type t = {
    mutable id : int;
    domain_name : string;
    node_name : string;
    beacon : Beacon.t;
  }

  module Data_bijection = struct
    type domain = t
    type codomain = string * string
    let f {domain_name; node_name; _} = (domain_name, node_name)
    let f_inv (domain_name, node_name) =
      {id = id_unknown; domain_name; node_name;
       beacon = Beacon.dummy}
    let beacon {beacon; _} = beacon
  end

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {id; _} = assert (id >= 0); id
    let f_inv id =
      {id; domain_name = ""; node_name = ""; beacon = Beacon.dummy}
    let beacon {beacon; _} = beacon
  end

  module Data_cache = Batyr_cache.Cache_of_bijection (Data_bijection)
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let data_cache = Data_cache.create 23
  let id_cache = Id_cache.create 23

  let create ~domain_name ?(node_name = "") () =
    Data_cache.merge data_cache
      (Beacon.embed Batyr_cache.Grade.basic
        (fun beacon -> {id = id_unknown; domain_name; node_name; beacon}))

  let domain_name {domain_name; _} = domain_name
  let node_name {node_name; _} = node_name
  let to_string node =
    if node.node_name = "" then node.domain_name else
    node.node_name ^ "@" ^ node.domain_name

  let equal = (==)
  let hash {domain_name; node_name; _} = Hashtbl.hash (domain_name, node_name)

  let cached_of_id id =
    try Some (Id_cache.find_key id_cache id) with Not_found -> None
  let cached_id {id; _} = if id >= 0 then Some id else None

  let stored_of_id id =
    try Lwt.return (Id_cache.find_key id_cache id)
    with Not_found ->
      Batyr_db.use_accounted_exn (Batyr_sql.Node.get id)
        >|= fun (cost, (domain_name, node_name)) ->
      let node =
        Beacon.embed cost
          (fun beacon -> {id; domain_name; node_name; beacon}) in
      try Id_cache.find id_cache node
      with Not_found ->
        Data_cache.add data_cache node;
        Id_cache.add id_cache node;
        node

  let stored_id node =
    if node.id >= 0 then Lwt.return (Some node.id) else
    if node.id = id_missing then Lwt.return_none else
    Batyr_db.use_accounted_exn
      (Batyr_sql.Node.locate node.domain_name node.node_name)
      >|= fun (cost, id_opt) ->
    Beacon.set_grade cost node.beacon;
    match id_opt with
    | None -> node.id <- id_missing; None
    | Some id -> node.id <- id; Id_cache.add id_cache node; Some id

  let store node =
    if node.id >= 0 then Lwt.return node.id else
    Batyr_db.use_accounted_exn
      (Batyr_sql.Node.store node.domain_name node.node_name)
      >|= fun (cost, id) ->
    Beacon.set_grade cost node.beacon;
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
    beacon : Beacon.t;
  }

  let dummy = {
    id = id_unknown;
    domain_name = "";
    node_name = "";
    resource_name = "";
    beacon = Beacon.dummy;
  }

  module Data_bijection = struct
    type domain = t
    type codomain = string * string * string
    let f {domain_name; node_name; resource_name; _} =
      (domain_name, node_name, resource_name)
    let f_inv (domain_name, node_name, resource_name) = {
      id = id_unknown; domain_name; node_name; resource_name;
      beacon = Beacon.dummy;
    }
    let beacon {beacon; _} = beacon
  end

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {id; _} = assert (id >= 0); id
    let f_inv id = {dummy with id}
    let beacon {beacon; _} = beacon
  end

  module Data_cache = Batyr_cache.Cache_of_bijection (Data_bijection)
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let data_cache = Data_cache.create 23
  let id_cache = Id_cache.create 23

  let create ~domain_name ?(node_name = "") ?(resource_name = "") () =
    Data_cache.merge data_cache
      (Beacon.embed Batyr_cache.Grade.basic
        (fun beacon ->
         {id = id_unknown; domain_name; node_name; resource_name; beacon}))

  let create_on_node node resource_name =
    create ~domain_name:(Node.domain_name node)
           ~node_name:(Node.node_name node) ~resource_name ()

  let domain_name {domain_name; _} = domain_name
  let node_name {node_name; _} = node_name
  let resource_name {resource_name; _} = resource_name
  let node {domain_name; node_name; _} = Node.create ~domain_name ~node_name ()

  let equal = (==)
  let hash {domain_name; node_name; resource_name; _} =
    Hashtbl.hash (domain_name, node_name, resource_name)

  let dummy_of_id id = {dummy with id}
  let cached_of_id id =
    try Some (Id_cache.find_key id_cache id) with Not_found -> None
  let cached_id {id; _} = if id >= 0 then Some id else None

  let stored_of_id id =
    try Lwt.return (Id_cache.find_key id_cache id)
    with Not_found ->
      Batyr_db.use_accounted_exn (Batyr_sql.Resource.get id)
        >|= fun (cost, (domain_name, node_name, resource_name)) ->
      let resource =
        Beacon.embed cost
          (fun beacon -> {id; domain_name; node_name; resource_name; beacon}) in
      try Id_cache.find id_cache resource
      with Not_found ->
        Data_cache.add data_cache resource;
        Id_cache.add id_cache resource;
        resource

  let stored_id resource =
    if resource.id >= 0 then Lwt.return (Some resource.id) else
    if resource.id = id_missing then Lwt.return_none else
    Batyr_db.use_accounted_exn
      (Batyr_sql.Resource.locate resource.domain_name resource.node_name
                                 resource.resource_name)
      >|= fun (cost, id_opt) ->
    Beacon.set_grade cost resource.beacon;
    match id_opt with
    | None -> resource.id <- id_missing; None
    | Some id -> resource.id <- id; Id_cache.add id_cache resource; Some id

  let store resource =
    if resource.id >= 0 then Lwt.return resource.id else
    Batyr_db.use_accounted_exn
      (Batyr_sql.Resource.store resource.domain_name resource.node_name
                                resource.resource_name)
      >|= fun (cost, id) ->
    Beacon.set_grade cost resource.beacon;
    resource.id <- id;
    Id_cache.add id_cache resource;
    id
end

module Account = struct
  type t = {
    mutable resource : Resource.t;
    mutable port : int;
    mutable password : string;
    mutable is_active : bool;
    beacon : Beacon.t;
  }

  module Id_bijection = struct
    type domain = t
    type codomain = int
    let f {resource; _} = Resource.cached_id resource |> Option.get
    let f_inv resource_id = {
      resource = Resource.dummy_of_id resource_id;
      port = 0; password = ""; is_active = false;
      beacon = Beacon.dummy;
    }
    let beacon {beacon; _} = beacon
  end
  module Id_cache = Batyr_cache.Cache_of_bijection (Id_bijection)
  let id_cache = Id_cache.create 11

  let create ~resource ?(port = 5222) ~password ?(is_active = false) () =
    let%lwt resource_id = Resource.store resource in
    let%lwt cost, () = (* OBS: Should be load cost. *)
      Batyr_db.use_accounted_exn
        (Batyr_sql.Account.create ~resource_id ~port ~password ~is_active) in
    Lwt.return @@ Beacon.embed cost
      (fun beacon -> {resource; port; password; is_active; beacon})

  let update ?resource ?port ?password ?is_active account =
    let id = Resource.cached_id account.resource |> Option.get in
    Batyr_db.use_exn @@ fun c ->
    (match resource with
      | None -> Lwt.return_ok ()
      | Some x when Resource.equal x account.resource -> Lwt.return_ok ()
      | Some x -> Id_cache.remove id_cache account;
                  let%lwt new_id = Resource.store x in
                  account.resource <- x;
                  Batyr_sql.Account.set_resource id new_id c >|=? fun () ->
                  Id_cache.add id_cache account) >>=?? fun () ->
    (match port with
      | None -> Lwt.return_ok ()
      | Some x when x = account.port -> Lwt.return_ok ()
      | Some x -> account.port <- x;
                  Batyr_sql.Account.set_port id x c) >>=?? fun () ->
    (match password with
      | None -> Lwt.return_ok ()
      | Some x when x = account.password -> Lwt.return_ok ()
      | Some x -> account.password <- x;
                  Batyr_sql.Account.set_password id x c) >>=?? fun () ->
    (match is_active with
      | None -> Lwt.return_ok ()
      | Some x when x = account.is_active -> Lwt.return_ok ()
      | Some x -> account.is_active <- x;
                  Batyr_sql.Account.set_is_active id x c)

  let delete_id resource_id =
    Batyr_db.use_exn (Batyr_sql.Account.delete resource_id)

  let delete account =
    (* TODO: deplete beacon *)
    let resource_id = Resource.cached_id account.resource |> Option.get in
    delete_id resource_id

  let of_resource resource =
    match%lwt Resource.stored_id resource with
    | None -> Lwt.return None
    | Some resource_id ->
      try Lwt.return (Some (Id_cache.find_key id_cache resource_id))
      with Not_found ->
        match%lwt
          Batyr_db.use_accounted_exn (Batyr_sql.Account.get resource_id) with
        | _, None -> Lwt.return_none
        | cost, Some (port, password, is_active) ->
          let%lwt resource = Resource.stored_of_id resource_id in
          let account = Beacon.embed cost
                (fun beacon -> {resource; port; password; is_active; beacon}) in
          Lwt.return (Some account)

  let merge cost (resource_id, port, password, is_active) =
    try Lwt.return (Id_cache.find_key id_cache resource_id)
    with Not_found ->
      Resource.stored_of_id resource_id >|= fun resource ->
      Beacon.embed cost
        (fun beacon -> {resource; port; password; is_active; beacon})

  let all () =
    let%lwt cost_all, rows = Batyr_db.use_accounted_exn Batyr_sql.Account.all in
    let cost = cost_all /. float_of_int (List.length rows) in
    Lwt_list.map_s (merge cost) rows

  let all_active () =
    let%lwt cost_all, rows =
      Batyr_db.use_accounted_exn Batyr_sql.Account.all_active in
    let cost = cost_all /. float_of_int (List.length rows) in
    Lwt_list.map_s (merge cost) rows

  let resource {resource; _} = resource
  let host {resource; _} = Resource.domain_name resource
  let port {port; _} = port
  let password {password; _} = password
  let is_active {is_active; _} = is_active

  let equal {resource = r0; _} {resource = r1; _} = Resource.equal r0 r1
  let hash {resource; _} = Resource.hash resource
end

module Muc_room = struct
  type t = {
    node : Node.t;
    alias : string option;
    description : string option;
    transcribe : bool;
    min_message_time : float option;
    beacon : Beacon.t;
  }
  type t' = t

  module Node_hashable = struct
    type t = t'
    let equal roomA roomB = Node.equal roomA.node roomB.node
    let hash {node; _} = Node.hash node
    let beacon {beacon; _} = beacon
  end

  module Node_cache = Batyr_cache.Cache_of_hashable (Node_hashable)
  let node_cache = Node_cache.create 23

  let node {node; _} = node
  let alias {alias; _} = alias
  let description {description; _} = description
  let transcribe {transcribe; _} = transcribe
  let min_message_time {min_message_time; _} = min_message_time

  let make_dummy node = {
    node; alias = None; description = None; transcribe = false;
    min_message_time = None;
    beacon = Beacon.dummy;
  }

  let cached_of_node node =
    try Some (Node_cache.find node_cache (make_dummy node))
    with Not_found -> None

  let stored_of_node node =
    try Lwt.return (Some (Node_cache.find node_cache (make_dummy node)))
    with Not_found ->
      begin match%lwt Node.stored_id node with
      | None -> Lwt.return_none
      | Some node_id ->
        Batyr_db.use_accounted_exn (Batyr_sql.Muc_room.stored_of_node node_id)
          >|= fun (cost, qr) ->
        Option.map
          (fun (alias, description, transcribe, mmt) ->
            let min_message_time =
              Option.map CalendarLib.Calendar.to_unixfloat mmt in
            let room =
              Beacon.embed cost (fun beacon ->
                {node; alias; description; transcribe; min_message_time;
                 beacon}) in
            Node_cache.merge node_cache room)
          qr
      end
end

type message_type = [`Normal | `Chat | `Groupchat | `Headline]

let string_of_message_type = function
 | `Normal -> "normal"
 | `Chat -> "chat"
 | `Groupchat -> "groupchat"
 | `Headline -> "headline"

module Message = struct
  type t = {
    seen_time : Ptime.t;
    sender : Resource.t;
    recipient : Resource.t;
    message_type : message_type;
    subject : string option;
    thread : string option;
    body : string option;
  }

  let seen_time {seen_time; _} = seen_time
  let make ~seen_time ~sender ~recipient
           ~message_type ?subject ?thread ?body () =
    {seen_time; sender; recipient; message_type; subject; thread; body}
  let sender {sender; _} = sender
  let recipient {recipient; _} = recipient
  let message_type {message_type; _} = message_type
  let subject {subject; _} = subject
  let thread {thread; _} = thread
  let body {body; _} = body

  let store ?muc_author msg =
    let author_id = Option.search Resource.cached_id muc_author in
    let%lwt sender_id = Resource.store (sender msg) in
    let%lwt recipient_id = Resource.store (recipient msg) in
    Batyr_db.use_exn @@
      Batyr_sql.Message.store
        (seen_time msg)
        sender_id author_id recipient_id
        (string_of_message_type (message_type msg))
        (subject msg)
        (thread msg)
        (body msg)
end
