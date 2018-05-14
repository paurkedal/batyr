(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
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
open Printf
open Unprime_option
open Unprime

let rtm_uri = Uri.of_string "https://slack.com/api/rtm.connect"

type channel = string
let string_of_channel = ident
let channel_of_string = ident

type team = string
let string_of_team = ident
let team_of_string = ident

type user = string
let string_of_user = ident
let user_of_string = ident

type user_info = {
  id: string;
  name: string;
}

type team_info = {
  domain: string;
  id: string;
  name: string;
}

type connect_response = {
  self: user_info;
  team: team_info;
  url: Uri.t;
}

type message = {
  subtype: string option;
  channel: channel;
  user: user;
  text: string;
  ts: Ptime.t;
  source_team: team option;
  team: team option;
}

type t = {
  team: team_info;
  user: user_info;
  receive: unit -> Websocket.Frame.t Lwt.t;
  send: Websocket.Frame.t -> unit Lwt.t;
}

type error = [`Msg of string]
type error_or_closed = [error | `Closed]

let decode_connect json =
  let open Kojson_pattern in
  Kojson.jin_of_json json |> K.assoc begin
    "ok"^: K.bool %> fun ok ->
      if ok then
        "self"^:
          K.assoc begin
            "id"^: K.string %> fun id ->
            "name"^: K.string %> fun name ->
            Ka.stop {id; name}
          end %> fun self ->
        "team"^:
          K.assoc begin
            "domain"^: K.string %> fun domain ->
            "id"^: K.string %> fun id ->
            "name"^: K.string %> fun name ->
            Ka.stop {domain; id; name}
          end %> fun team ->
        "url"^: K.convert_string "Uri.t" Uri.of_string %> fun url ->
        Ka.stop (Ok {self; team; url})
      else
        "error"^: K.string %> fun msg ->
        Ka.stop (Error (`Msg msg))
  end

let connect_ws resp =
  let uri = resp.url in
  let connect_to endp =
    let%lwt receive, send = Websocket_lwt.with_connection endp uri in
    Lwt.return_ok {team = resp.team; user = resp.self; receive; send} in
  (match Uri.scheme uri, Uri.host uri with
   | Some "wss", Some host ->
      let%lwt resolver = Dns_resolver_unix.create () in
      let%lwt ips = Dns_resolver_unix.gethostbyname resolver host in
      if ips = [] then
        Lwt.return_error (`Msg ("Host " ^ host ^ " not found."))
      else
        let ip = List.nth ips (Random.int (List.length ips)) in
        let port = Option.get_or 443 (Uri.port uri) in
        connect_to (`TLS (`Hostname host, `IP ip, `Port port))
   | _ ->
      Lwt.return_error (`Msg ("Unexpected WebSocket URI " ^ Uri.to_string uri)))

let connect ~token () =
  let q = [
    "token", [token];
    "batch_presence_aware", ["1"];
  ] in
  let uri = Uri.with_query rtm_uri q in
  let%lwt resp, body = Cohttp_lwt_unix.Client.call `GET uri in
  (match Cohttp.Response.status resp with
   | `OK ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      (match body |> Yojson.Basic.from_string |> decode_connect with
       | Ok resp -> connect_ws resp
       | Error _ as r -> Lwt.return r)
   | status ->
      let msg =
        sprintf "HTTP response %s" (Cohttp.Code.string_of_status status) in
      Lwt.return_error (`Msg msg))

let team_info conn = conn.team
let user_info conn = conn.user

module Frame = Websocket.Frame
module Opcode = Websocket.Frame.Opcode

let send_json conn json =
  let content = Yojson.Basic.to_string json in
  conn.send (Frame.create ~opcode:Opcode.Text ~content ())

let rec receive_text conn =
  let%lwt frame = conn.receive () in
  (match frame.Frame.opcode with
   | Opcode.Continuation -> assert false (* TODO *)
   | Opcode.Text -> Lwt.return_ok frame.Frame.content
   | Opcode.Binary -> receive_text conn
   | Opcode.Close -> Lwt.return_error `Closed
   | Opcode.Ping ->
      Logs_lwt.info (fun m -> m "ping-pong") >>= fun () ->
      conn.send (Frame.create ~opcode:Opcode.Pong ()) >>= fun () ->
      receive_text conn
   | Opcode.Pong -> assert false (* TODO: If we need to ping. *)
   | Opcode.Ctrl _ | Opcode.Nonctrl _ -> receive_text conn)

let receive_json conn =
  (match%lwt receive_text conn with
   | Ok msg -> Lwt.return_ok (Yojson.Basic.from_string msg)
   | Error err -> Lwt.return_error err)

let ptime_of_string ts =
  let s, ps =
    (match String.split_on_char '.' ts with
     | [s] | [s; ""] ->
        (int_of_string s, 0L)
     | [s; s'] ->
        let n' = String.length s' in
        let n', s' = if n' <= 12 then 12, s' else n', String.sub s' 0 12 in
        let ps_scale = Prime_int64.pow 10L (12 - (String.length s')) in
        let ps = Int64.(mul (of_string s') ps_scale) in
        (int_of_string s, ps)
     | _ -> failwith "Slack_rtm.ptime_of_string") in
  let d = s / 86400 in
  let ps = Int64.(add ps (mul (of_int (s mod 86400)) 1_000_000_000_000L)) in
  Ptime.v (d, ps)

let disconnect conn =
  let content = {|{"type": "goodbye"}|} in
  conn.send (Frame.create ~opcode:Opcode.Text ~content ()) >>= fun () ->
  conn.send (Frame.close 1001)

let message_of_json json =
  let open Kojson_pattern in
  Kojson.jin_of_json json |> K.assoc begin
    "type"^: K.string %> function
     | "message" ->
        "subtype"^?: Option.map K.string %> fun subtype ->
        "channel"^: K.string %> fun channel ->
        "user"^: K.string %> fun user ->
        "text"^: K.string %> fun text ->
        "ts"^: K.convert_string "timestamp" ptime_of_string %> fun ts ->
        "source_team"^?: Option.map K.string %> fun source_team ->
        "team"^?: Option.map K.string %> fun team ->
        Ka.stop (`Message {subtype; channel; user; text; ts; source_team; team})
     | _ -> Ka.stop `Unknown
  end

let rec receive conn =
  (match%lwt receive_json conn with
   | Ok json ->
      (match message_of_json json with
       | exception Kojson.Mismatch (path, expectation) ->
          let msg = Kojson.string_of_mismatch (path, expectation)
                  ^ " while parsing " ^ Yojson.Basic.to_string json in
          Lwt.return_error (`Msg msg)
       | (`Message _ as msg) -> Lwt.return_ok msg
       | `Unknown -> receive conn)
   | Error _ as r -> Lwt.return r)
