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

open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Syntax

type ('a, 'b) t = 'a -> ('b, string) result Lwt.t

let make_call endpoint encode_request decode_response request =
  let req = request |> encode_request |> Yojson.Safe.to_string in
  let+ resp =
    XmlHttpRequest.perform_raw endpoint
      ~response_type:XmlHttpRequest.Text
      ~override_mime_type:"application/json"
      ~content_type:"application/json"
      ~contents:(`String req)
  in
  (match resp.code with
   | 200 ->
      resp.content
        |> Js.to_string
        |> Yojson.Safe.from_string
        |> decode_response
   | 0 ->
      Fmt.error "HTTP request to %s failed, server down?" endpoint
   | code ->
      Fmt.error "HTTP request to %s failed with HTTP status %d." endpoint code)

let count_messages =
  make_call Api_protocol.count_messages_path
    Api_protocol.count_messages_request_to_yojson
    Api_protocol.count_messages_response_of_yojson

let fetch_messages =
  make_call Api_protocol.fetch_messages_path
    Api_protocol.fetch_messages_request_to_yojson
    Api_protocol.fetch_messages_response_of_yojson
