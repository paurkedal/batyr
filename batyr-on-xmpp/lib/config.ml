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

type t = {
  storage_uri: Uri.t;
  resource: string;
  port: int;
  password: string;
  log_level: Batyr_core.Logging.Verbosity.t;
}

module Decode = Decoders_yojson.Basic.Decode

let decoder =
  let open Decode in
  let* storage_uri = field "storage_uri" string >|= Uri.of_string in
  let* resource = field "resource" string in
  let* port = field_opt_or ~default:5222 "port" int in
  let* password = field "password" string in
  let+ log_level =
    let* s = field_opt_or ~default:"info" "log_level" string in
    (match Batyr_core.Logging.Verbosity.of_string s with
     | Ok log_level -> succeed log_level
     | Error (`Msg msg) -> fail msg)
  in
  {storage_uri; resource; port; password; log_level}

let load path =
  let+ content = Lwt_io.with_file ~mode:Lwt_io.input path Lwt_io.read in
  (match Decode.decode_value decoder (Yojson.Basic.from_string content) with
   | Ok _ as r -> r
   | Error msg ->
      Fmt.error_msg "Cannot load %s: %a" path Decode.pp_error msg
   | exception Yojson.Json_error msg ->
      Fmt.error_msg "Cannot load %s: %s" path msg)

let verbosity config = config.log_level
