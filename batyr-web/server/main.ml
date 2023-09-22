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

open Lwt.Infix
open Lwt.Syntax

open Content

module Log_auth = (val Logs_lwt.src_log (Logs.Src.create "batyr.auth"))

let index _ =
  let* rooms = Data.rooms () in
  let render_room (_node_id, domain_name, node_name, alias, _transcribe) =
    let node_jid = node_name ^ "@" ^ domain_name in
    let label =
      (match alias with
       | None -> [H.txt node_jid]
       | Some alias -> [H.txt alias; H.txt " <"; H.txt node_jid; H.txt ">"])
    in
    H.li [H.a ~a:[H.a_href (Vpaths.room node_jid)] label]
  in
  let ul = H.ul (List.map render_room rooms) in
  page ~title:"Room Index" [ul]

let unauthorized msg =
  Content.error_page ~status:`Unauthorized ~title:"Unauthorized" msg

let pp_jwt =
  Fmt.(using (fun jwt -> Yojson.Safe.to_string jwt.Jose.Jwt.payload) string)

let authenticated =
  (match Config.global.bearer_jwk with
   | None -> Fun.id
   | Some jwk ->
      fun handler request ->
        (match Dream.header request "Authorization" with
         | None -> unauthorized "Missing authorization header."
         | Some data ->
            (match String.split_on_char ' ' data |> List.filter ((<>) "") with
             | ["Bearer"; token] ->
                let now = Ptime_clock.now () in
                (match Jose.Jwt.of_string ~jwk ~now token with
                 | Ok jwt ->
                    Log_auth.info (fun f -> f "Auth %a" pp_jwt jwt)
                      >>= fun () ->
                    handler request
                 | Error `Expired ->
                    unauthorized "The bearer token has expired."
                 | Error `Invalid_signature ->
                    unauthorized "The signature of the bearer token is invalid."
                 | Error (`Msg msg) ->
                    unauthorized ("Bad bearer token: " ^ msg)
                 | Error `Not_json ->
                    unauthorized "Bearer token data is not JSON."
                 | Error `Not_supported ->
                    unauthorized "Bearer token format unsupported.")
             | _ ->
                unauthorized "Authorization header is not a bearer token.")))

let () =
  Dream.run
    ?interface:Config.global.listen_interface
    ?port:Config.global.listen_port
    ?tls:Config.global.tls_enabled
    ?certificate_file:Config.global.tls_certificate_file
    ?key_file:Config.global.tls_key_file
    @@
  Dream.logger @@
  Dream.router [
    Dream.scope "/" [authenticated] [
      Dream.get Vpaths.index index;
      Dream.get (Vpaths.room ":room_jid") Room.handle;
      Dream.get (Vpaths.static "**") (Dream.static Config.(global.static_dir));
      Dream.post Api_protocol.count_messages_path Api.handle_count_messages;
      Dream.post Api_protocol.fetch_messages_path Api.handle_fetch_messages;
    ]
  ]
