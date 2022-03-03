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

let dump_json json =
  (match Unix.getenv "BATYR_DEBUG_DUMP_DIR" with
   | exception Not_found -> Lwt.return_unit
   | dir ->
      let data = Yojson.Basic.to_string json in
      let digest = data
        |> Cryptokit.(hash_string (Hash.md5 ()))
        |> Cryptokit.(transform_string (Hexa.encode ()))
      in
      let path =
        Filename.concat dir (Printf.sprintf "undecodable-%s.json" digest)
      in
      let* () = Log.info (fun f -> f "Saving undecodable reponse to %s" path) in
      Lwt_io.(with_file ~mode:output) path (fun oc -> Lwt_io.write oc data))
