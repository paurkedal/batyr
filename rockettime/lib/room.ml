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
module Decode = Decoders_yojson.Basic.Decode

type direct_chat_room = {
  id: string;
}

let direct_chat_room_decoder =
  let open Decode in
  let+ id = field "_id" string in
  {id}

type chat_room = {
  id: string;
  name: string;
  u: User.t option;
  topic: string option;
  muted: string list;
  jitsi_timeout: Ptime.t option;
}

let chat_room_decoder =
  let open Decode in
  let* id = field "_id" string in
  let* name = field "name" string in
  let* u = field_opt "u" User.option_decoder >|= Option.join in
  let* topic = field_opt "topic" string in
  let* muted = field_opt "muted" (list string) >|= Option.value ~default:[] in
  let+ jitsi_timeout = field_opt "jitsiTimeout" ptime_decoder in
  {id; name; u; topic; muted; jitsi_timeout}

type private_chat_room = {
  id: string;
  name: string;
  u: User.t option;
  topic: string option;
  muted: string list;
  jitsi_timeout: Ptime.t option;
  ro: bool;
}

let private_chat_room_decoder =
  let open Decode in
  let* id = field "_id" string in
  let* name = field "name" string in
  let* u = field_opt "u" User.option_decoder >|= Option.join in
  let* topic = field_opt "topic" string in
  let* muted = field_opt "muted" (list string) >|= Option.value ~default:[] in
  let* jitsi_timeout = field_opt "jitsiTimeout" ptime_decoder in
  let+ ro = field "ro" bool in
  {id; name; u; topic; muted; jitsi_timeout; ro}

type livechat_room = {
  id: string;
  (* undocumented *)
}

let livechat_room_decoder =
  let open Decode in
  let+ id = field "_id" string in
  {id}

type t =
  | Direct_chat_room of direct_chat_room
  | Chat_room of chat_room
  | Private_room of private_chat_room
  | Livechat_room of livechat_room

let decoder =
  let open Decode in
  let* t = field "t" string in
  (match t with
   | "d" -> let+ d = direct_chat_room_decoder in Direct_chat_room d
   | "c" -> let+ d = chat_room_decoder in Chat_room d
   | "p" -> let+ d = private_chat_room_decoder in Private_room d
   | "l" -> let+ d = livechat_room_decoder in Livechat_room d
   | _ -> fail "Unrecognized room type.")

let id = function
 | Direct_chat_room {id; _}
 | Chat_room {id; _}
 | Private_room {id; _}
 | Livechat_room {id; _} -> id

let name = function
 | Direct_chat_room room -> room.id
 | Chat_room room -> room.name
 | Private_room room -> room.name
 | Livechat_room room -> room.id
