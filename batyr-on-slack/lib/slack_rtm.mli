(* Copyright (C) 2018--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

type add_message = {
  subtype: string option;
  user: user;
  text: string;
  source_team: team option;
  team: team option;
}

type previous_message = {
  ts: Ptime.t;
  subtype: string option;
  user: user;
  text: string;
}

type edited_message = {
  ts: Ptime.t;
  subtype: string option;
  user: user;
  text: string;
  edited_user: user;
  edited_ts: Ptime.t;
}

type change_message = {
  event_ts: Ptime.t option;
  previous_message: previous_message;
  message: edited_message;
}

type delete_message = {
  event_ts: Ptime.t option;
  previous_message: previous_message;
}

type message_subevent =
  [ `Add of add_message
  | `Change of change_message
  | `Delete of delete_message
  | `Other of Yojson.Basic.t ]

type message_event = {
  channel: channel;
  ts: Ptime.t;
  sub: message_subevent;
}

type t
type error = [`Msg of string]
type error_or_closed = [error | `Closed]

val connect :
  dns_client: Dns_client_lwt.t ->
  token: string ->
  ?ping_period: Ptime.Span.t ->
  ?ping_patience: Ptime.Span.t ->
  unit -> (t, error) result Lwt.t

val disconnect : t -> unit Lwt.t

val team_info : t -> team_info

val user_info : t -> user_info

val send_json : t -> Yojson.Basic.t -> unit Lwt.t

val receive_json : t -> (Yojson.Basic.t, error_or_closed) result Lwt.t

val receive : t -> (message_event, error_or_closed) result Lwt.t
