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
open Protocol_conv_jsonm

let jsonm_errorf = Log.jsonm_errorf

module Duration = struct
  type t = Ptime.Span.t

  let of_jsonm_exn = function
   | `Float t_s as t_json ->
      (match Ptime.Span.of_float_s t_s with
       | None -> jsonm_errorf t_json "duration out of range"
       | Some t -> t)
   | json ->
      jsonm_errorf json "expecting float time"

  let to_jsonm t = `Float (Ptime.Span.to_float_s t)
end

type t = {
  log_level: Log.level_option [@default Some Logs.Warning];
  storage_uri: string;
  slack_token: string;
  slack_bot_token: string option [@default None];
  slack_ping_period: Duration.t option [@default None];
  slack_ping_patience: Duration.t option [@default None];
}
[@@deriving protocol ~driver:(module Jsonm)]

let load config_path =
  let* config_string =
    Lwt_io.with_file ~mode:Lwt_io.input config_path Lwt_io.read in
  let+ config_json = Lwt.wrap1 Ezjsonm.from_string config_string in
  (match of_jsonm (Ezjsonm.value config_json) with
   | Ok config ->
      Ok config
   | Error err ->
      let msg = Protocol_conv_jsonm.Jsonm.error_to_string_hum err in
      Error (`Msg msg))
