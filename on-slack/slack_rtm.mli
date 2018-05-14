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

(** Slack RTM. *)

type team
val team_of_string : string -> team
val string_of_team : team -> string

type channel
val channel_of_string : string -> channel
val string_of_channel : channel -> string

type user
val user_of_string : string -> user
val string_of_user : user -> string

type team_info = {
  domain: string;
  id: string;
  name: string;
}

type user_info = {
  id: string;
  name: string;
}

type message = {
  subtype: string option;
  channel: channel;
  user: user;
  text: string;
  ts: Ptime.t;
  source_team: team option;
  team: team option;
}

type t
type error = [`Msg of string]
type error_or_closed = [error | `Closed]

val connect : token: string -> unit -> (t, error) result Lwt.t

val disconnect : t -> unit Lwt.t

val team_info : t -> team_info

val user_info : t -> user_info

val send_json : t -> Yojson.Basic.json -> unit Lwt.t

val receive_json : t -> (Yojson.Basic.json, error_or_closed) result Lwt.t

val receive : t -> ([`Message of message], error_or_closed) result Lwt.t
