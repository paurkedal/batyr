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
  listen_interface: string option;
  listen_port: int option;
  tls_enabled: bool option;
  tls_certificate_file: string option;
  tls_key_file: string option;
  site_prefix: string;
  storage_uri: Uri.t;
  static_dir: string;
}

let decoder =
  let open Decode in
  let* listen_interface = field_opt "listen_interface" string in
  let* listen_port = field_opt "listen_port" int in
  let* tls_enabled = field_opt "tls_enabled" bool in
  let* tls_certificate_file = field_opt "tls_certificate_file" string in
  let* tls_key_file = field_opt "tls_key_file" string in
  let* site_prefix = field_opt_or ~default:"" "site_prefix" string in
  let* storage_uri = field "storage_uri" string in
  let+ static_dir = field "static_dir" string in
  {
    listen_interface;
    listen_port;
    tls_enabled;
    tls_certificate_file;
    tls_key_file;
    site_prefix;
    storage_uri = Uri.of_string storage_uri;
    static_dir;
  }

let global = Lwt_main.run begin
  let open Lwt.Syntax in
  let path =
    try Sys.getenv "BATYR_WEB_CONFIG" with Not_found -> "/etc/batyr-web.json"
  in
  let+ content = Lwt_io.with_file ~mode:Lwt_io.input path Lwt_io.read in
  (match Decode.decode_value decoder (Yojson.Basic.from_string content) with
   | Ok v -> v
   | Error msg ->
      Fmt.failwith "Cannot load %s: %a" path Decode.pp_error msg
   | exception Yojson.Json_error msg ->
      Fmt.failwith "Cannot load %s: %s" path msg)
end
