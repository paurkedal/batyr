(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix
open Printf
open Unprime_list
module type CONNECTION = Caqti_lwt.CONNECTION

module Caqti_type = struct
  include Caqti_type
  include Caqti_type_calendar
end

let missing_id tn id =
  ksprintf (fun s -> Lwt.fail (Failure s)) "Missing # %d of %s." id tn
let required tn id = function
  | None -> missing_id "batyr.domains" id
  | Some r -> Lwt.return r

module Node = struct
  let get_q = Caqti_request.find
    Caqti_type.int
    Caqti_type.(tup2 string string)
    "SELECT domain_name, node_name \
     FROM batyr.nodes NATURAL JOIN batyr.domains WHERE node_id = ?"
  let locate_q = Caqti_request.find_opt
    Caqti_type.(tup2 string string)
    Caqti_type.int
    "SELECT node_id FROM batyr.nodes NATURAL JOIN batyr.domains \
     WHERE domain_name = ? AND node_name = ?"
  let store_q = Caqti_request.find
    Caqti_type.(tup2 string string)
    Caqti_type.int
    "SELECT batyr.make_node(?, ?)"

  let get id (module C : CONNECTION) = C.find get_q id
  let locate domain_name node_name (module C : CONNECTION) =
    C.find_opt locate_q (domain_name, node_name)
  let store domain_name node_name (module C : CONNECTION) =
    C.find store_q (domain_name, node_name)
end

module Resource = struct
  let get_q = Caqti_request.find
    Caqti_type.int
    Caqti_type.(tup3 string string string)
    "SELECT domain_name, node_name, resource_name \
     FROM batyr.resources NATURAL JOIN batyr.nodes NATURAL JOIN batyr.domains \
     WHERE resource_id = ?"
  let locate_q = Caqti_request.find_opt
    Caqti_type.(tup3 string string string)
    Caqti_type.int
    "SELECT resource_id \
     FROM batyr.resources NATURAL JOIN batyr.nodes NATURAL JOIN batyr.domains \
     WHERE domain_name = ? AND node_name = ? AND resource_name = ?"
  let store_q = Caqti_request.find
    Caqti_type.(tup3 string string string)
    Caqti_type.int
    "SELECT batyr.make_resource(?, ?, ?)"

  let get id (module C : CONNECTION) =
    C.find get_q id
  let locate domain_name node_name resource_name (module C : CONNECTION) =
    C.find_opt locate_q (domain_name, node_name, resource_name)
  let store domain_name node_name resource_name (module C : CONNECTION) =
    C.find store_q (domain_name, node_name, resource_name)
end

module Account = struct

  let get_q = Caqti_request.find_opt
    Caqti_type.int
    Caqti_type.(tup3 int string bool)
    "SELECT server_port, client_password, is_active \
     FROM batyr.accounts WHERE resource_id = ?"
  let get resource_id (module C : CONNECTION) =
    C.find_opt get_q resource_id

  let all_q = Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup4 int int string bool)
    "SELECT resource_id, server_port, client_password, is_active \
     FROM batyr.accounts"

  let all_active_q = Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup4 int int string bool)
    "SELECT resource_id, server_port, client_password, is_active \
     FROM batyr.accounts WHERE is_active = true"

  let fetch_list q (module C : CONNECTION) = C.fold q List.cons () []
  let all conn = fetch_list all_q conn
  let all_active conn = fetch_list all_active_q conn

  let create_q = Caqti_request.exec
    Caqti_type.(tup4 int int string bool)
    "INSERT INTO batyr.accounts \
      (resource_id, server_port, client_password, is_active) \
     VALUES (?, ?, ?, ?)"
  let create ~resource_id ~port ~password ~is_active (module C : CONNECTION) =
    C.exec create_q (resource_id, port, password, is_active)

  let delete_q =
    Caqti_request.exec Caqti_type.int
    "DELETE FROM batyr.accounts WHERE resource_id = ?"
  let delete resource_id (module C : CONNECTION) =
    C.exec delete_q resource_id

  let set_resource_q = Caqti_request.exec
    Caqti_type.(tup2 int int)
    "UPDATE batyr.accounts SET resource_id = ? WHERE resource_id = ?"
  let set_port_q = Caqti_request.exec
    Caqti_type.(tup2 int int)
    "UPDATE batyr.accounts SET server_port = ? WHERE resource_id = ?"
  let set_password_q = Caqti_request.exec
    Caqti_type.(tup2 string int)
    "UPDATE batyr.accounts SET client_password = ? WHERE resource_id = ?"
  let set_is_active_q = Caqti_request.exec
    Caqti_type.(tup2 bool int)
    "UPDATE batyr.accounts SET is_active = ? WHERE resource_id = ?"

  let set_resource id x (module C : CONNECTION) =
    C.exec set_resource_q (x, id)
  let set_port id x (module C : CONNECTION) =
    C.exec set_port_q (x, id)
  let set_password id x (module C : CONNECTION) =
    C.exec set_password_q (x, id)
  let set_is_active id x (module C : CONNECTION) =
    C.exec set_is_active_q (x, id)
end

module Muc_room = struct
  let stored_of_node_q = Caqti_request.find_opt
    Caqti_type.int
    Caqti_type.(tup4 (option string) (option string) bool (option ctime))
    "SELECT room_alias, room_description, transcribe, \
            (SELECT min(seen_time) \
              FROM batyr.messages \
              JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
                ON sender_id = sender.resource_id \
             WHERE node_id = $1) \
     FROM batyr.muc_rooms WHERE node_id = $1"
  let stored_of_node node_id (module C : CONNECTION) =
    C.find_opt stored_of_node_q node_id

  let latest_message_time_q = Caqti_request.find
    Caqti_type.int Caqti_type.(option ptime)
    "SELECT max(seen_time)
      FROM batyr.messages
      JOIN (batyr.resources NATURAL JOIN batyr.nodes) AS sender \
        ON sender_id = sender.resource_id
     WHERE node_id = ?"
  let latest_message_time node_id (module C : CONNECTION) =
    C.find latest_message_time_q node_id >|=
    (function
     | Ok (Some t) -> Ok (Some (Ptime.to_float_s t))
     | Ok None -> Ok None
     | Error _ as r -> r)
end

module Presence = struct
  let insert_muc_message_q = Caqti_request.exec
    Caqti_type.(tup2
      (tup4 ctime int (option int) int)
      (tup4 string (option string) (option string) (option string)))
    "INSERT INTO batyr.messages \
      (seen_time, sender_id, author_id, recipient_id, \
       message_type, subject, thread, body) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  let insert_muc_message seen_time sender_id author_id recipient_id
                         message_type subject thread body
                         (module C : CONNECTION) =
    C.exec insert_muc_message_q
      ((seen_time, sender_id, author_id, recipient_id),
       (message_type, subject, thread, body))

  let room_presence_q = Caqti_request.collect
    Caqti_type.int
    Caqti_type.(tup3 int (option string) (option ctime))
    "SELECT resource_id, nick, \
            (SELECT max(seen_time) \
               FROM batyr.messages JOIN batyr.resources AS sender \
                 ON sender_id = sender.resource_id \
             WHERE sender.node_id = node_id) \
     FROM batyr.muc_presence NATURAL JOIN batyr.resources \
     WHERE account_id = ? AND is_present = true"
  let room_presence account_id (module C : CONNECTION) =
    C.fold room_presence_q List.cons account_id []

  let active_accounts_q = Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup3 int int string)
    "SELECT resource_id, server_port, client_password \
     FROM batyr.accounts NATURAL JOIN batyr.resources \
     WHERE is_active = true"
  let active_accounts (module C : CONNECTION) =
    C.fold active_accounts_q List.cons () []
end

module Web = struct
  let rooms_q = Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup2 (tup4 int string string (option string)) bool)
    "SELECT DISTINCT node_id, domain_name, node_name, room_alias, \
                     transcribe \
     FROM batyr.muc_rooms NATURAL JOIN batyr.nodes \
                          NATURAL JOIN batyr.domains \
     ORDER BY domain_name DESC, node_name DESC, room_alias DESC"
  let rooms (module C : CONNECTION) =
    C.fold rooms_q
      (fun ((node_id, domain_name, node_name, room_alias), transcribe) ->
        List.cons (node_id, domain_name, node_name, room_alias, transcribe))
      () []
end

module Admin = struct
  let fetch_chatrooms_q = Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup4 int (option string) (option string) bool)
    "SELECT node_id, room_alias, room_description, transcribe \
     FROM batyr.muc_rooms"
  let fetch_chatrooms (module C : CONNECTION) =
    C.fold fetch_chatrooms_q List.cons () []

  let update_chatroom_q = Caqti_request.exec
    Caqti_type.(tup4 int (option string) (option string) bool)
    "UPDATE batyr.muc_rooms \
     SET room_alias = $2, room_description = $3, transcribe = $4 \
     WHERE node_id = $1"
  let insert_chatroom_q = Caqti_request.exec
    Caqti_type.(tup4 int (option string) (option string) bool)
    "INSERT INTO batyr.muc_rooms \
      (node_id, room_alias, room_description, transcribe) \
     VALUES ($1, $2, $3, $4)"
  let upsert_chatroom do_ins node_id room_alias room_description transcribe
                      (module C : CONNECTION) =
    C.exec (if do_ins then insert_chatroom_q else update_chatroom_q)
           (node_id, room_alias, room_description, transcribe)
  let delete_chatroom_q = Caqti_request.exec Caqti_type.int
    "DELETE FROM batyr.muc_rooms WHERE node_id = $1"
  let delete_chatroom node_id (module C : CONNECTION) =
    C.exec delete_chatroom_q node_id

  let fetch_presences_q = Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup4 string string bool (option string))
    "SELECT resource.jid, account.jid, is_present, nick \
     FROM batyr.muc_presence NATURAL JOIN batyr.resource_jids AS resource \
                               INNER JOIN batyr.resource_jids AS account \
                                       ON account_id = account.resource_id"
  let fetch_presences (module C : CONNECTION) =
    C.fold fetch_presences_q List.cons () []

  let update_presence_q = Caqti_request.exec
    Caqti_type.(tup4 int int (option string) bool)
    "UPDATE batyr.muc_presence \
     SET account_id = $2, nick = $3, is_present = $4 \
     WHERE resource_id = $1"
  let insert_presence_q = Caqti_request.exec
    Caqti_type.(tup4 int int (option string) bool)
    "INSERT INTO batyr.muc_presence \
        (resource_id, account_id, nick, is_present) \
     VALUES ($1, $2, $3, $4)"
  let upsert_presence do_ins resource_id account_id nick is_present
                      (module C : CONNECTION) =
    C.exec (if do_ins then insert_presence_q else update_presence_q)
           (resource_id, account_id, nick, is_present)

  let delete_presence_q = Caqti_request.exec
    Caqti_type.int
    "DELETE FROM batyr.muc_presence WHERE resource_id = $1"
  let delete_presence resource_id (module C : CONNECTION) =
    C.exec delete_presence_q resource_id
end
