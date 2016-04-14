(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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
open Batyrweb_server

let json_of_option f = function
  | None -> `Null
  | Some x -> f x
let json_of_string (x : string) = `String x
let json_of_float (x : float) = `Float x

let room_info (node_id, domain_name, node_name, room_alias) =
  lwt lmt = Batyr_db.use (Batyr_sql.Muc_room.latest_message_time node_id) in
  let info : Yojson.Basic.json = `Assoc [
    "alias", json_of_option json_of_string room_alias;
    "latest_message_time", json_of_option json_of_float lmt;
  ] in
  Lwt.return (node_name, info)

let status_handler () () =
  lwt rooms = Batyr_db.use Batyr_sql.Web.rooms in
  lwt assoc = Lwt_list.map_s room_info rooms in
  Lwt.return (Yojson.Basic.pretty_to_string (`Assoc assoc), "application/json")

let () = Eliom_registration.String.register status_service status_handler
