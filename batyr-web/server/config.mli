(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

type t = {
  listen_interface: string option;
  listen_port: int option;
  tls_enabled: bool option;
  tls_certificate_file: string option;
  tls_key_file: string option;
  site_prefix: string;  (* default: "" *)
  storage_uri: Uri.t;   (* required *)
  static_dir: string;   (* required *)
  bearer_jwk: Jose.Jwk.public Jose.Jwk.t option;
}

val global : t
