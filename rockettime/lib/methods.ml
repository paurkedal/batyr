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
open Types
module Decode = Connection.Decode
module Encode = Connection.Encode

let ( let*? ) = Lwt_result.Syntax.( let* )

let yojson_of_option f = function
 | None -> `Null
 | Some x -> f x

(* Login *)

type login_response = {
  id: string;
  token: string;
  token_expires: Ptime.t option;
  type_: string option;
}

let login_decoder =
  let open Decode in
  let* id = field "id" string in
  let* token = field "token" string in
  let* token_expires = field "tokenExpires" ptime_option_decoder in
  let+ type_ = field_opt "type" string in
  {id; token; token_expires; type_}

let login_with_password ~username ~password conn =
  let digest =
    password
      |> Cryptokit.(hash_string (Hash.sha256 ()))
      |> Cryptokit.(transform_string (Hexa.encode ()))
  in
  Connection.call conn ~decoder:login_decoder
    "login" [
      `Assoc [
        "user", `Assoc [
          "username", `String username;
        ];
        "password", `Assoc [
          "digest", `String digest;
          "algorithm", `String "sha-256";
        ];
      ]
    ]

let resume_with_token ~token conn =
  Connection.call conn ~decoder:login_decoder
    "login" [
      `Assoc ["resume", `String token];
    ]

(* Join Channel *)

let join_channel ~room_id ?join_code conn =
  Connection.call conn ~decoder:Decode.bool
    "joinRoom" [
      `String room_id;
      yojson_of_option (fun s -> `String s) join_code;
    ]

(* Get Rooms *)

type get_rooms_response = {
  update: Room.t list;
  remove: string list; (* room IDs *)
}

let get_rooms_decoder =
  let open Decode in
  let* update = field "update" (list Room.decoder) in
  let+ remove = field "remove" (list string) in
  {update; remove}

let get_rooms ?(since = Ptime.epoch) conn =
  Connection.call conn ~decoder:get_rooms_decoder
    "rooms/get" [ptime_encoder since]

(* Load History *)

type load_history_response = {
  messages: Message.t list;
  unread_not_loaded: int;
}

let load_history_decoder =
  let open Decode in
  let* messages = field "messages" (list Message.decoder) in
  let+ unread_not_loaded = field "unreadNotLoaded" int in
  {messages; unread_not_loaded}

let load_history_prim ~room_id ?until ~count ~since conn =
  Connection.call conn ~decoder:load_history_decoder
    "loadHistory" Encode.[
      string room_id;
      nullable ptime_encoder until;
      int count;
      ptime_encoder since;
    ]

let load_history ~room_id ~since ?until conn =
  let until = match until with Some t -> t | None -> Ptime_clock.now () in
  let*? resp1 = load_history_prim ~room_id ~since ~until ~count:1 conn in
  if resp1.unread_not_loaded = 0 then
    Log.debug (fun f ->
      f "loadHistory 1-step: %d, %d"
        (List.length resp1.messages) resp1.unread_not_loaded) >|= fun () ->
    Ok resp1
  else
    let count = resp1.unread_not_loaded + 1 in
    let*? resp2 = load_history_prim ~room_id ~since ~until ~count conn in
    Log.debug (fun f ->
      f "loadHistory 2-step: %d, %d; %d, %d"
        (List.length resp1.messages) resp1.unread_not_loaded
        (List.length resp2.messages) resp2.unread_not_loaded) >|= fun () ->
    Ok resp2
