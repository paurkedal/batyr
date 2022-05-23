(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
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
open Caqti_type.Std
open Caqti_request.Infix

module type CONNECTION = Caqti_lwt.CONNECTION

module Muc_room = struct
  let latest_message_time_q =
    Caqti_type.int -->! Caqti_type.(option ptime) @:-
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

module Web = struct
  let rooms_q =
    Caqti_type.unit -->*
    Caqti_type.(tup2 (tup4 int string string (option string)) bool) @:-
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
  let fetch_chatrooms_q =
    Caqti_type.unit -->*
    Caqti_type.(tup4 int (option string) (option string) bool) @:-
    "SELECT node_id, room_alias, room_description, transcribe \
     FROM batyr.muc_rooms"
  let fetch_chatrooms (module C : CONNECTION) =
    C.fold fetch_chatrooms_q List.cons () []

  let update_chatroom_q =
    Caqti_type.(tup4 int (option string) (option string) bool) -->. unit @:-
    "UPDATE batyr.muc_rooms \
     SET room_alias = $2, room_description = $3, transcribe = $4 \
     WHERE node_id = $1"
  let insert_chatroom_q =
    Caqti_type.(tup4 int (option string) (option string) bool) -->. unit @:-
    "INSERT INTO batyr.muc_rooms \
      (node_id, room_alias, room_description, transcribe) \
     VALUES ($1, $2, $3, $4)"
  let upsert_chatroom do_ins node_id room_alias room_description transcribe
                      (module C : CONNECTION) =
    C.exec (if do_ins then insert_chatroom_q else update_chatroom_q)
           (node_id, room_alias, room_description, transcribe)
  let delete_chatroom_q =
    Caqti_type.int -->. unit @:-
    "DELETE FROM batyr.muc_rooms WHERE node_id = $1"
  let delete_chatroom node_id (module C : CONNECTION) =
    C.exec delete_chatroom_q node_id

  let fetch_presences_q =
    Caqti_type.unit -->*
    Caqti_type.(tup4 string string bool (option string)) @:-
    "SELECT resource.jid, account.jid, is_present, nick \
     FROM batyr.muc_presence NATURAL JOIN batyr.resource_jids AS resource \
                               INNER JOIN batyr.resource_jids AS account \
                                       ON account_id = account.resource_id"
  let fetch_presences (module C : CONNECTION) =
    C.fold fetch_presences_q List.cons () []

  let update_presence_q =
    Caqti_type.(tup4 int int (option string) bool) -->. unit @:-
    "UPDATE batyr.muc_presence \
     SET account_id = $2, nick = $3, is_present = $4 \
     WHERE resource_id = $1"
  let insert_presence_q =
    Caqti_type.(tup4 int int (option string) bool) -->. unit @:-
    "INSERT INTO batyr.muc_presence \
        (resource_id, account_id, nick, is_present) \
     VALUES ($1, $2, $3, $4)"
  let upsert_presence do_ins resource_id account_id nick is_present
                      (module C : CONNECTION) =
    C.exec (if do_ins then insert_presence_q else update_presence_q)
           (resource_id, account_id, nick, is_present)

  let delete_presence_q =
    Caqti_type.int -->. unit @:-
    "DELETE FROM batyr.muc_presence WHERE resource_id = $1"
  let delete_presence resource_id (module C : CONNECTION) =
    C.exec delete_presence_q resource_id
end
