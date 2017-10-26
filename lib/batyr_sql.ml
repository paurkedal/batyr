(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
open Caqti_lwt
open Printf
open Unprime_list

let missing_id tn id =
  ksprintf (fun s -> Lwt.fail (Failure s)) "Missing # %d of %s." id tn
let required tn id = function
  | None -> missing_id "batyr.domains" id
  | Some r -> Lwt.return r

open Caqti_query

module Node = struct
  let get' = prepare_fun @@ function
    | `Pgsql -> "SELECT domain_name, node_name \
                 FROM batyr.nodes NATURAL JOIN batyr.domains WHERE node_id = $1"
    | _ -> raise Missing_query_string
  let locate' = prepare_fun @@ function
    | `Pgsql -> "SELECT node_id FROM batyr.nodes NATURAL JOIN batyr.domains \
                 WHERE domain_name = $1 AND node_name = $2"
    | _ -> raise Missing_query_string
  let store' = prepare_fun @@ function
    | `Pgsql -> "SELECT batyr.make_node($1, $2)"
    | _ -> raise Missing_query_string

  let get id (module C : CONNECTION) =
    C.find get' C.Tuple.(fun t -> (string 0 t, string 1 t))
           C.Param.[|int id|]
  let locate domain_name node_name (module C : CONNECTION) =
    C.find_opt locate' C.Tuple.(fun t -> int 0 t)
               C.Param.[|string domain_name; string node_name|]
  let store domain_name node_name (module C : CONNECTION) =
    C.find store' C.Tuple.(fun t -> int 0 t)
           C.Param.[|string domain_name; string node_name|]
end

module Resource = struct
  let get' = prepare_fun @@ function
    | `Pgsql -> "SELECT domain_name, node_name, resource_name \
                 FROM batyr.resources NATURAL JOIN batyr.nodes \
                                      NATURAL JOIN batyr.domains \
                 WHERE resource_id = $1"
    | _ -> raise Missing_query_string
  let locate' = prepare_fun @@ function
    | `Pgsql -> "SELECT resource_id \
                 FROM batyr.resources NATURAL JOIN batyr.nodes \
                                      NATURAL JOIN batyr.domains \
                 WHERE domain_name = $1 AND node_name = $2 \
                                        AND resource_name = $3"
    | _ -> raise Missing_query_string
  let store' = prepare_fun @@ function
    | `Pgsql -> "SELECT batyr.make_resource($1, $2, $3)"
    | _ -> raise Missing_query_string

  let get id (module C : CONNECTION) =
    C.find get' C.Tuple.(fun t -> (string 0 t, string 1 t, string 2 t))
           C.Param.[|int id|]
  let locate domain_name node_name resource_name (module C : CONNECTION) =
    C.find_opt locate' C.Tuple.(int 0)
        C.Param.[|string domain_name; string node_name; string resource_name|]
  let store domain_name node_name resource_name (module C : CONNECTION) =
    C.find store' C.Tuple.(int 0)
        C.Param.[|string domain_name; string node_name; string resource_name|]
end

module Account = struct
  let get' = prepare_fun @@ function
    | `Pgsql -> "SELECT server_port, client_password, is_active \
                 FROM batyr.accounts WHERE resource_id = $1"
    | _ -> raise Missing_query_string

  let get resource_id (module C : CONNECTION) =
    C.find_opt get' C.Tuple.(fun t -> (int 0 t, string 1 t, bool 2 t))
               C.Param.[|int resource_id|]

  let all' = prepare_sql
    "SELECT resource_id, server_port, client_password, is_active \
     FROM batyr.accounts"

  let all_active' = prepare_sql
    "SELECT resource_id, server_port, client_password, is_active \
     FROM batyr.accounts WHERE is_active = true"

  let fetch_list query (module C : CONNECTION) =
    C.fold query
        C.Tuple.(fun t acc -> (int 0 t, int 1 t, string 2 t, bool 3 t) :: acc)
        [||] []
  let all = fetch_list all'
  let all_active = fetch_list all_active'

  let create' = prepare_fun @@ function
    | `Pgsql -> "INSERT INTO batyr.accounts \
                  (resource_id, server_port, client_password, is_active) \
                 VALUES ($1, $2, $3, $4)"
    | _ -> raise Missing_query_string

  let create ~resource_id ~port ~password ~is_active (module C : CONNECTION) =
    C.exec create'
        C.Param.[|int resource_id; int port; string password; bool is_active|]

  let delete' = prepare_fun @@ function
    | `Pgsql -> "DELETE FROM batyr.accounts WHERE resource_id = $1"
    | _ -> raise Missing_query_string

  let delete resource_id (module C : CONNECTION) =
    C.exec delete' C.Param.[|int resource_id|]

  let set_resource' = prepare_fun @@ function
    | `Pgsql ->
      "UPDATE batyr.accounts SET resource_id = $1 WHERE resource_id = $2"
    | _ -> raise Missing_query_string
  let set_port' = prepare_fun @@ function
    | `Pgsql ->
      "UPDATE batyr.accounts SET server_port = $1 WHERE resource_id = $2"
    | _ -> raise Missing_query_string
  let set_password' = prepare_fun @@ function
    | `Pgsql ->
      "UPDATE batyr.accounts SET client_password = $1 WHERE resource_id = $2"
    | _ -> raise Missing_query_string
  let set_is_active' = prepare_fun @@ function
    | `Pgsql ->
      "UPDATE batyr.accounts SET is_active = $1 WHERE resource_id = $2"
    | _ -> raise Missing_query_string

  let set_resource id x (module C : CONNECTION) =
    C.exec set_resource' C.Param.[|int x; int id|]
  let set_port id x (module C : CONNECTION) =
    C.exec set_port' C.Param.[|int x; int id|]
  let set_password id x (module C : CONNECTION) =
    C.exec set_password' C.Param.[|string x; int id|]
  let set_is_active id x (module C : CONNECTION) =
    C.exec set_is_active' C.Param.[|bool x; int id|]
end

module Muc_room = struct
  let stored_of_node' = prepare_fun @@ function
    | `Pgsql ->
      "SELECT room_alias, room_description, transcribe, \
              (SELECT min(seen_time) \
                FROM batyr.messages \
                JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
                  ON sender_id = sender.resource_id \
               WHERE node_id = $1) \
       FROM batyr.muc_rooms WHERE node_id = $1"
    | _ -> raise Missing_query_string
  let stored_of_node node_id (module C : CONNECTION) =
    C.find_opt stored_of_node'
      C.Tuple.(fun t -> option string 0 t, option string 1 t,
                        bool 2 t, option utc 3 t)
      C.Param.[|int node_id|]

  let latest_message_time' = prepare_sql_p
    "SELECT max(seen_time)
      FROM batyr.messages
      JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
        ON sender_id = sender.resource_id
     WHERE node_id = ?"
  let latest_message_time node_id (module C : CONNECTION) =
    C.find_opt latest_message_time'
               C.Tuple.(option utc_float 0) C.Param.[|int node_id|]
      >|= Prime_option.flatten
end

module Presence = struct
  let insert_muc_message' = prepare_fun @@ function
    | `Pgsql ->
      "INSERT INTO batyr.messages \
        (seen_time, sender_id, author_id, recipient_id, \
         message_type, subject, thread, body) \
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
    | _ -> raise Missing_query_string
  let insert_muc_message seen_time sender_id author_id recipient_id
                         message_type subject thread body
                         (module C : CONNECTION) =
    C.exec insert_muc_message' C.Param.[|
      utc seen_time;
      int sender_id;
      option int author_id;
      int recipient_id;
      string message_type;
      option string subject;
      option string thread;
      option string body;
    |]

  let room_presence' = prepare_fun @@ function
    | `Pgsql ->
      "SELECT resource_id, nick, \
              (SELECT max(seen_time) \
                 FROM batyr.messages JOIN batyr.resources AS sender \
                   ON sender_id = sender.resource_id \
               WHERE sender.node_id = node_id) \
       FROM batyr.muc_presence NATURAL JOIN batyr.resources \
       WHERE account_id = $1 AND is_present = true"
    | _ -> raise Missing_query_string
  let room_presence account_id (module C : CONNECTION) =
    C.fold room_presence'
           C.Tuple.(fun t -> List.cons (int 0 t, option string 1 t,
                                        option utc 2 t))
           C.Param.[|int account_id|] []

  let active_accounts' = prepare_fun @@ function
    | `Pgsql -> "SELECT resource_id, server_port, client_password \
                 FROM batyr.accounts NATURAL JOIN batyr.resources \
                 WHERE is_active = true"
    | _ -> raise Missing_query_string
  let active_accounts (module C : CONNECTION) =
    C.fold active_accounts'
           C.Tuple.(fun t -> List.cons (int 0 t, int 1 t, string 2 t))
           [||] []
end

module Web = struct
  let rooms' = prepare_fun @@ function
    | `Pgsql ->
      "SELECT DISTINCT node_id, domain_name, node_name, room_alias, \
                       transcribe \
       FROM batyr.muc_rooms NATURAL JOIN batyr.nodes \
                            NATURAL JOIN batyr.domains \
       ORDER BY domain_name DESC, node_name DESC, room_alias DESC"
    | _ -> raise Missing_query_string
  let rooms (module C : CONNECTION) =
    C.fold rooms' C.Tuple.(fun t -> List.cons (int 0 t, string 1 t, string 2 t,
                                               option string 3 t, bool 4 t))
           [||] []
end

module Admin = struct
  let fetch_chatrooms' = prepare_fun @@ function
    | `Pgsql ->
      "SELECT node_id, room_alias, room_description, transcribe \
       FROM batyr.muc_rooms"
    | _ -> raise Missing_query_string
  let fetch_chatrooms (module C : CONNECTION) =
    C.fold fetch_chatrooms'
           C.Tuple.(fun t -> List.cons (int 0 t, option string 1 t,
                                        option string 2 t, bool 3 t))
           [||] []

  let update_chatroom' = prepare_fun @@ function
    | `Pgsql ->
      "UPDATE batyr.muc_rooms \
       SET room_alias = $2, room_description = $3, transcribe = $4 \
       WHERE node_id = $1"
    | _ -> raise Missing_query_string
  let insert_chatroom' = prepare_fun @@ function
    | `Pgsql ->
      "INSERT INTO batyr.muc_rooms \
        (node_id, room_alias, room_description, transcribe) \
       VALUES ($1, $2, $3, $4)"
    | _ -> raise Missing_query_string
  let upsert_chatroom do_ins node_id room_alias room_description transcribe
                      (module C : CONNECTION) =
    C.exec (if do_ins then insert_chatroom' else update_chatroom')
           C.Param.[|int node_id; option string room_alias;
                     option string room_description; bool transcribe|]
  let delete_chatroom' = prepare_fun @@ function
    | `Pgsql -> "DELETE FROM batyr.muc_rooms WHERE node_id = $1"
    | _ -> raise Missing_query_string
  let delete_chatroom node_id (module C : CONNECTION) =
    C.exec delete_chatroom' C.Param.[|int node_id|]

  let fetch_presences' = prepare_fun @@ function
    | `Pgsql ->
      "SELECT resource.jid, account.jid, is_present, nick \
       FROM batyr.muc_presence NATURAL JOIN batyr.resource_jids AS resource \
                                 INNER JOIN batyr.resource_jids AS account \
                                         ON account_id = account.resource_id"
    | _ -> raise Missing_query_string
  let fetch_presences (module C : CONNECTION) =
    C.fold fetch_presences'
           C.Tuple.(fun t -> List.cons (string 0 t, string 1 t, bool 2 t,
                                        option string 3 t))
           [||] []

  let update_presence' = prepare_fun @@ function
    | `Pgsql ->
      "UPDATE batyr.muc_presence \
       SET account_id = $2, nick = $3, is_present = $4 \
       WHERE resource_id = $1"
    | _ -> raise Missing_query_string
  let insert_presence' = prepare_fun @@ function
    | `Pgsql ->
      "INSERT INTO batyr.muc_presence \
          (resource_id, account_id, nick, is_present) \
       VALUES ($1, $2, $3, $4)"
    | _ -> raise Missing_query_string
  let upsert_presence do_ins resource_id account_id nick is_present
                      (module C : CONNECTION) =
    C.exec (if do_ins then insert_presence' else update_presence')
           C.Param.[|int resource_id; int account_id;
                     option string nick; bool is_present|]

  let delete_presence' = prepare_fun @@ function
    | `Pgsql ->
      "DELETE FROM batyr.muc_presence WHERE resource_id = $1"
    | _ -> raise Missing_query_string
  let delete_presence resource_id (module C : CONNECTION) =
    C.exec delete_presence' C.Param.[|int resource_id|]
end
