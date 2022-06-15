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

type t = {
  site_prefix: string;
  storage_uri: Uri.t;
  static_dir: string;
}

let decoder =
  let open Decode in
  let* site_prefix = field_opt_or ~default:"" "site_prefix" string in
  let* storage_uri = field "storage_uri" string in
  let+ static_dir = field "static_dir" string in
  {site_prefix; storage_uri = Uri.of_string storage_uri; static_dir}

let global = Lwt_main.run begin
  let open Lwt.Syntax in
  let path =
    try Sys.getenv "BATYR_CONFIG" with Not_found -> "/etc/batyr.json"
  in
  let+ content = Lwt_io.with_file ~mode:Lwt_io.input path Lwt_io.read in
  (match Decode.decode_value decoder (Yojson.Basic.from_string content) with
   | Ok v -> v
   | Error msg ->
      Fmt.failwith "Cannot load %s: %a" path Decode.pp_error msg
   | exception Yojson.Json_error msg ->
      Fmt.failwith "Cannot load %s: %s" path msg)
end
