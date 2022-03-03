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

open Printf
open Unprime_list
module type CONNECTION = Caqti_lwt.CONNECTION

type connection = (module CONNECTION)

type error = Caqti_error.t

module Caqti_type = struct
  include Caqti_type
  include Caqti_type_calendar
end

let missing_id tn id =
  ksprintf (fun s -> Lwt.fail (Failure s)) "Missing # %d of %s." id tn
let required _tn id = function
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
end

module Message = struct
  let store_q = Caqti_request.exec
    Caqti_type.(tup2
      (tup4 ptime int (option int) int)
      (tup4 string (option string) (option string) (option string)))
    "INSERT INTO batyr.messages \
      (seen_time, sender_id, author_id, recipient_id, \
       message_type, subject, thread, body) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  let store seen_time sender_id author_id recipient_id
            message_type subject thread body
            (module C : CONNECTION) =
    C.exec store_q
      ((seen_time, sender_id, author_id, recipient_id),
       (message_type, subject, thread, body))
end
