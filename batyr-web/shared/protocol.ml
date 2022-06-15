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

(** Protocol for /api endpoints. *)

(* Common *)

type ptime = Ptime.t

let ptime_of_yojson = function
 | `Float t ->
    (match Ptime.of_float_s t with
     | Some t -> Ok t
     | None -> Error "ptime_of_yojson")
 | _ -> Error "ptime_of_yojson"

let ptime_to_yojson t = `Float (Ptime.to_float_s t)


(* Message Counts *)

let count_messages_path = Vpaths.api_endpoint "count_messages"

type count_messages_request = {
  room: string; (* JID *)
  pattern: string option [@default None];
  tz: string [@default "UTC"];
}
[@@deriving yojson]

type message_count = {
  date: int; (* year lsl 16 lor month lsl 8 lor monthday *)
  count: int;
}
[@@deriving yojson]

type count_messages_response = message_count list
[@@deriving yojson]


(* Fetch Messages *)

let fetch_messages_path = Vpaths.api_endpoint "fetch_messages"

type fetch_messages_request = {
  room: string;
  time_start: float option [@default None];
  time_stop: float option [@default None];
  pattern: string option [@default None];
}
[@@deriving yojson]

type message = {
  msg_time: ptime;
  msg_edit_time: ptime option;
  msg_sender_cls: string;
  msg_sender: string;
  msg_subject: string option;
  msg_thread: string option;
  msg_body: string option;
}
[@@deriving yojson]

type fetch_messages_response = message list
[@@deriving yojson]
