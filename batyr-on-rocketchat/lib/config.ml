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

open Lwt.Syntax

module Duration = struct
  type t = Ptime.Span.t

  let of_yojson = function
   | `Int t_s -> Ok (Ptime.Span.of_int_s t_s)
   | `Float t_s ->
      (match Ptime.Span.of_float_s t_s with
       | None -> Error "duration out of range"
       | Some t -> Ok t)
   | _ ->
      Error "expecting float time"

  let to_yojson t = `Float (Ptime.Span.to_float_s t)
end

type nameserver = {
  host: string;
  port: int;
}
[@@deriving yojson]

type t = {
  log_level: Log.level_option [@default Some Logs.Warning];
  storage_uri: string;
  rocketchat_uri: string;
  rocketchat_user: string option [@default None];
  rocketchat_token: string;
  rocketchat_ping_period: Duration.t [@default Ptime.Span.v (240, 0L)];
  rocketchat_ping_patience: Duration.t [@default Ptime.Span.v (600, 0L)];
  include_rooms: string list option;
  nameservers: nameserver list [@default [{host = "127.0.0.53"; port = 53}]];
}
[@@deriving yojson]

let load config_path =
  let* config_string =
    Lwt_io.with_file ~mode:Lwt_io.input config_path Lwt_io.read
  in
  let* config_json = Lwt.wrap1 Yojson.Safe.from_string config_string in
  (match of_yojson config_json with
   | Ok config -> Lwt.return_ok config
   | Error msg -> Lwt.return_error (`Msg msg))
