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

open Lwt.Infix

let pp_json_briefly ppf v =
  let max_string_length = 30 in
  let max_list_length = 5 in
  let max_assoc_length = 12 in

  let rec take ellipses n = function
   | [] -> []
   | _ :: _ when n = 1 -> [ellipses]
   | x :: xs -> x :: take ellipses (n - 1) xs
  in
  let leading_list = take (`String "...") max_list_length in
  let leading_assoc = take ("...", `String "...") max_assoc_length in

  let rec abbrev = function
   | `Null | `Bool _ | `Int _ | `Float _ as v -> v
   | `String s ->
      `String begin
        if String.length s <= max_string_length then s else
        String.sub s 0 (max_string_length - 5) ^ " [...]"
      end
   | `Assoc kvs ->
      let abbrev' (k, v) = (k, abbrev v) in
      `Assoc (List.map abbrev' (leading_assoc kvs))
   | `List vs ->
      `List (List.map abbrev (leading_list vs))
  in
  Format.pp_print_string ppf (Yojson.Basic.to_string (abbrev v))

let dump_json json =
  (match Unix.getenv "BATYR_DEBUG_DUMP_DIR" with
   | exception Not_found ->
      Log.info (fun f ->
        f "The undecodable response is: %a" pp_json_briefly json)
   | dir ->
      let data = Yojson.Basic.to_string json in
      let digest = data
        |> Cryptokit.(hash_string (Hash.md5 () [@alert "-crypto"]))
        |> Cryptokit.(transform_string (Hexa.encode ()))
      in
      let path =
        Filename.concat dir (Printf.sprintf "undecodable-%s.json" digest)
      in
      Log.info (fun f ->
        f "Saving undecodable reponse to %s." path) >>= fun () ->
      Lwt_io.(with_file ~mode:output) path (fun oc -> Lwt_io.write oc data))
