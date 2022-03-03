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

module Decode = Decoders_yojson.Basic.Decode
module Encode = Decoders_yojson.Basic.Encode

type json = Yojson.Basic.t

type operational_error = {
  msg: string;
  reason: string;
}
[@@deriving show]

type result_error = {
  is_client_safe: bool;
  error: string;
  message: string;
  reason: string;
  error_type: string;
}
[@@deriving show]

type error = [
  | `Msg of string
  | `Operational_error of operational_error
  | `Result_error of result_error
  | `Protocol_error of string
  | `Cannot_decode of Decode.error * json
]

val pp_error : Format.formatter -> error -> unit

type t

val connect :
  dns_client: Dns_client_lwt.t ->
  ?call_delay: Ptime.Span.t ->
  Uri.t -> (t, [> error]) result Lwt.t

val uri : t -> Uri.t

val call :
  decoder: 'a Decode.decoder ->
  t -> string -> json list -> ('a, [> error]) result Lwt.t

type room_messages_event =
  | Update_messages of Message.t list
  | Delete_messages of string list

val subscribe_to_room_messages :
  on_event: (room_messages_event -> unit Lwt.t) ->
  room: Room.t ->
  t -> (unit, [> error]) result Lwt.t
