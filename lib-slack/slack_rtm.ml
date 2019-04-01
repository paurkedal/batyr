(* Copyright (C) 2018--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

let ptime_of_string ts =
  let s, ps =
    (match String.split_on_char '.' ts with
     | [s] | [s; ""] ->
        (int_of_string s, 0L)
     | [s; s'] ->
        let s' = if String.length s' <= 12 then s' else String.sub s' 0 12 in
        let ps_scale = Prime_int64.pow 10L (12 - (String.length s')) in
        let ps = Int64.(mul (of_string s') ps_scale) in
        (int_of_string s, ps)
     | _ -> failwith "Slack_rtm.ptime_of_string") in
  let d = s / 86400 in
  let ps = Int64.(add ps (mul (of_int (s mod 86400)) 1_000_000_000_000L)) in
  Ptime.v (d, ps)

open Kojson_pattern
module K = struct
  include Kojson_pattern.K
  let ptime = Kojson_pattern.K.convert_string "timestamp" ptime_of_string
end

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

type add_message = {
  subtype: string option;
  user: user;
  text: string;
  source_team: team option;
  team: team option;
}

let add_message_decoder subtype =
  "user"^: K.string %> fun user ->
  "text"^: K.string %> fun text ->
  "source_team"^?: Option.map K.string %> fun source_team ->
  "team"^?: Option.map K.string %> fun team ->
  Ka.stop (`Add {subtype; user; text; source_team; team})

type previous_message = {
  ts: Ptime.t;
  subtype: string option;
  user: user;
  text: string;
}

let previous_message_decoder =
  K.assoc begin
    "ts"^: K.ptime %> fun ts ->
    "subtype"^?: Option.map K.string %> fun subtype ->
    "user"^: K.string %> fun user ->
    "text"^: K.string %> fun text ->
    Ka.stop {ts; subtype; user; text}
  end

type edited_message = {
  ts: Ptime.t;
  subtype: string option;
  user: user;
  text: string;
  edited_user: user;
  edited_ts: Ptime.t;
}

let edited_message_decoder =
  K.assoc begin
    "ts"^: K.ptime %> fun ts ->
    "subtype"^?: Option.map K.string %> fun subtype ->
    "user"^: K.string %> fun user ->
    "text"^: K.string %> fun text ->
    "edited"^:
      K.assoc begin
        "ts"^: K.ptime %> fun ts ->
        "user"^: K.string %> fun user ->
        Ka.stop (ts, user)
      end %> fun (edited_ts, edited_user) ->
    Ka.stop {ts; subtype; user; text; edited_ts; edited_user}
  end

type change_message = {
  event_ts: Ptime.t option;
  previous_message: previous_message;
  message: edited_message;
}

let change_message_decoder =
  "event_ts"^?: Option.map K.ptime %> fun event_ts ->
  "message"^: edited_message_decoder %> fun message ->
  "previous_message"^: previous_message_decoder %> fun previous_message ->
  Ka.stop (`Change {event_ts; message; previous_message})

type delete_message = {
  event_ts: Ptime.t option;
  previous_message: previous_message;
}

let delete_message_decoder =
  "event_ts"^?: Option.map K.ptime %> fun event_ts ->
  "previous_message"^: previous_message_decoder %> fun previous_message ->
  Ka.stop (`Delete {event_ts; previous_message})

type message_subevent =
  [ `Add of add_message
  | `Change of change_message
  | `Delete of delete_message
  | `Other of Yojson.Basic.json ]

type message_event = {
  channel: channel;
  ts: Ptime.t;
  sub: message_subevent;
}

let message_of_json json =
  Kojson.jin_of_json json |> K.assoc begin
    "type"^: K.string %> function
     | "message" ->
        "channel"^: K.string %> fun channel ->
        "ts"^: K.ptime %> fun ts ->
        "subtype"^?: Option.map K.string %> fun subtype ->
        (match subtype with
         | None | Some "me_message" as subtype -> add_message_decoder subtype
         | Some "message_changed" -> change_message_decoder
         | Some "message_deleted" -> delete_message_decoder
         | Some _ -> Ka.stop (`Other json)) %> fun sub ->
        `Message_event {channel; ts; sub}
     | _ ->
        Ka.stop `Other_event
  end

type t = {
  team: team_info;
  user: user_info;
  receive: unit -> Websocket.Frame.t Lwt.t;
  send: Websocket.Frame.t -> unit Lwt.t;
  ping_period: Ptime.Span.t;
  ping_patience: Ptime.Span.t;
  mutable latest_ping: Ptime.t;
}

type error = [`Msg of string]
type error_or_closed = [error | `Closed]

let decode_connect json =
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

let default_ping_period = Ptime.Span.of_int_s 240
let default_ping_patience = Ptime.Span.of_int_s 600

let connect_ws ~ping_period ~ping_patience resp =
  let uri = resp.url in
  let connect_to endp =
    let%lwt receive, send = Websocket_lwt.with_connection endp uri in
    Lwt.return_ok {
      team = resp.team;
      user = resp.self;
      receive;
      send;
      ping_period;
      ping_patience;
      latest_ping = Ptime_clock.now ();
    } in
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

let connect
    ~token
    ?(ping_period = default_ping_period)
    ?(ping_patience = default_ping_patience) () =
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
       | Ok resp -> connect_ws ~ping_period ~ping_patience resp
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

let rec wake_from_limbo conn =
  Lwt_unix.sleep (Ptime.Span.to_float_s conn.ping_period) >>= fun () ->
  let ping_age = Ptime.diff (Ptime_clock.now ()) conn.latest_ping in
  if Ptime.Span.compare ping_age conn.ping_period < 0 then
    wake_from_limbo conn else
  if Ptime.Span.compare ping_age conn.ping_patience < 0 then begin
    Log.debug (fun m ->
      m "Sent ping; non seen for %a" Ptime.Span.pp ping_age) >>= fun () ->
    conn.send (Frame.create ~opcode:Opcode.Ping ()) >>= fun () ->
    wake_from_limbo conn
  end else
  Log.err (fun m -> m
    "Issuing EOF; no ping seen for %a" Ptime.Span.pp ping_age) >>= fun () ->
  Lwt.fail End_of_file

let rec receive_text conn =
  conn.latest_ping <- Ptime_clock.now ();
  try%lwt
    let%lwt frame = Lwt.pick [
      conn.receive ();
      wake_from_limbo conn;
    ] in
    (match frame.Frame.opcode with
     | Opcode.Continuation -> assert false (* TODO *)
     | Opcode.Text -> Lwt.return_ok frame.Frame.content
     | Opcode.Binary -> receive_text conn
     | Opcode.Close -> Lwt.return_error `Closed
     | Opcode.Ping ->
        conn.latest_ping <- Ptime_clock.now ();
        Log.info (fun m -> m "Received ping.") >>= fun () ->
        conn.send (Frame.create ~opcode:Opcode.Pong ()) >>= fun () ->
        receive_text conn
     | Opcode.Pong ->
        Log.info (fun m -> m "Received pong.") >>= fun () ->
        conn.latest_ping <- Ptime_clock.now ();
        receive_text conn
     | Opcode.Ctrl _ | Opcode.Nonctrl _ -> receive_text conn)
  with End_of_file ->
    Log.err (fun m ->
      m "Unexpeted end of file from Slack RTM, will be handled as if closed.")
      >>= fun () ->
    Lwt.return_error `Closed

let receive_json conn =
  (match%lwt receive_text conn with
   | Ok msg -> Lwt.return_ok (Yojson.Basic.from_string msg)
   | Error err -> Lwt.return_error err)

let disconnect conn =
  let content = {|{"type": "goodbye"}|} in
  conn.send (Frame.create ~opcode:Opcode.Text ~content ()) >>= fun () ->
  conn.send (Frame.close 1001)

let rec receive conn =
  (match%lwt receive_json conn with
   | Ok json ->
      (match message_of_json json with
       | exception Kojson.Mismatch (path, expectation) ->
          let msg = Kojson.string_of_mismatch (path, expectation)
                  ^ " while parsing " ^ Yojson.Basic.to_string json in
          Lwt.return_error (`Msg msg)
       | `Message_event msg -> Lwt.return_ok msg
       | `Other_event -> receive conn)
   | Error _ as r -> Lwt.return r)
