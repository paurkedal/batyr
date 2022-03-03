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

open Types

type direct_chat_room = {
  id: string;
}

type chat_room = {
  id: string;
  name: string;
  u: User.t option;
  topic: string option;
  muted: string list;
  jitsi_timeout: Ptime.t option;
}

type private_chat_room = {
  id: string;
  name: string;
  u: User.t option;
  topic: string option;
  muted: string list;
  jitsi_timeout: Ptime.t option;
  ro: bool;
}

type livechat_room = {
  id: string;
  (* undocumented *)
}

type t =
  | Direct_chat_room of direct_chat_room
  | Chat_room of chat_room
  | Private_room of private_chat_room
  | Livechat_room of livechat_room

val decoder : t decoder

val id : t -> string

val name : t -> string
