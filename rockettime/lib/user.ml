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

type t = {
  uid: string;
  username: string;
  name: string option;
}

let decoder =
  let open Decode in
  let* uid = field "_id" string in
  let* username = field "username" string in
  let+ name = field_opt "name" string in
  {uid; username; name}

(* "... may return a null user". An example under "Private chat" suggests that
 * the null user is { "_id": null, "username": null }. *)
let option_decoder =
  let open Decode in
  let* uid = field "_id" (nullable string) in
  let* username = field "username" (nullable string) in
  let* name = field_opt "name" string in
  (match uid, username with
   | None, None -> succeed None
   | Some uid, Some username -> succeed (Some {uid; username; name})
   | Some _, None | None, Some _ -> fail "User and null user chimera found.")
