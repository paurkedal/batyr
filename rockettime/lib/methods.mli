(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

type login_response = {
  id: string;
  token: string;
  token_expires: Ptime.t option;
  type_: string option;
}

val login_with_password :
  username: string ->
  password: string ->
  Connection.t -> (login_response, [> Connection.error]) result Lwt.t

val resume_with_token :
  token: string ->
  Connection.t -> (login_response, [> Connection.error]) result Lwt.t

val join_channel :
  room_id: string ->
  ?join_code: string ->
  Connection.t -> (bool, [> Connection.error]) result Lwt.t

type get_rooms_response = {
  update: Room.t list;
  remove: string list; (* room IDs *)
}

val get_rooms :
  ?since: Ptime.t ->
  Connection.t -> (get_rooms_response, [> Connection.error]) result Lwt.t

type load_history_response = {
  messages: Message.t list;
  unread_not_loaded: int [@key "unreadNotLoaded"];
}

val load_history :
  room_id: string ->
  since: Ptime.t ->
  ?until: Ptime.t ->
  Connection.t -> (load_history_response, [> Connection.error]) result Lwt.t
