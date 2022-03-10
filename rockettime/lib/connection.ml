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
open Lwt.Syntax
open Printf
open Unprime_option
open Unprime_string
module Decode = Decoders_yojson.Basic.Decode
module Encode = Decoders_yojson.Basic.Encode

let ( >>=? ) = Lwt_result.Infix.( >>= )
let ( let*? ) = Lwt_result.Syntax.( let* )
let ( let+? ) = Lwt_result.Syntax.( let+ )

type json = Yojson.Basic.t

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

type connected = {
  msg: string;
  session: string;
}

let connected_decoder =
  let open Decode in
  let* msg = field "msg" string in
  let+ session = field "session" string in
  {msg; session}

type operational_error = {
  msg: string;
  reason: string;
}
[@@deriving show]

let operational_error_decoder =
  let open Decode in
  let* msg = field "msg" string in
  let+ reason = field "reason" string in
  {msg; reason}

type result_error = {
  is_client_safe: bool;
  error: string;
  message: string;
  reason: string;
  error_type: string;
}
[@@deriving show]

let result_error_decoder =
  let open Decode in
  let* is_client_safe = field "isClientSafe" bool in
  let* error = field "error" string in
  let* message = field "message" string in
  let* reason = field "reason" string in
  let+ error_type = field "errorType" string in
  {is_client_safe; error; message; reason; error_type}

type error = [
  | `Msg of string
  | `Operational_error of operational_error
  | `Result_error of result_error
  | `Protocol_error of string
  | `Cannot_decode of Decode.error * json
]

let pp_error ppf = function
 | `Msg msg -> Format.pp_print_string ppf msg
 | `Operational_error err -> pp_operational_error ppf err
 | `Result_error err -> pp_result_error ppf err
 | `Protocol_error msg -> Format.pp_print_string ppf msg
 | `Cannot_decode (err, data) ->
    Format.fprintf ppf "%a. Failed to decode %a"
      Decode.pp_error err pp_json_briefly data

type call_response = {
  msg: string;
  id: string;
  result_or_error: (json, result_error) result;
}

let call_response_decoder =
  let open Decode in
  let* msg = field "msg" string in
  if msg <> "result" then fail "Result-response expected" else
  let* id = field "id" string in
  let* result = field_opt "result" value in
  let* error = field_opt "error" result_error_decoder in
  (match result, error with
   | Some v, None -> succeed {msg; id; result_or_error = Ok v}
   | None, Some e -> succeed {msg; id; result_or_error = Error e}
   | Some _, Some _ ->
      fail "Conflicting result and error fields in call response."
   | None, None ->
      fail "Expected result or error field in call response.")

let ready_decoder =
  let open Decode in
  let* msg = field "msg" string in
  if msg <> "ready" then fail "Ready-response expected" else
  let* subs = field "subs" (list string) in
  succeed subs

type room_messages_event =
  | Update_messages of Message.t list
  | Delete_messages of string list

let changed_decoder =
  let open Decode in
  let* msg = field "msg" string in
  if msg <> "changed" then fail "Changed-response expected" else
  let* _id = field "id" string in (* useless *)
  let* collection = field "collection" string in
  field "fields" begin
    let* event_name = field "eventName" string in
    field "args"
      (match collection with
       | "stream-room-messages" ->
          let+ messages = list Message.decoder in
          (event_name, Update_messages messages)
       | "stream-notify-room" ->
          (match String.split_on_char '/' event_name with
           | [room_id; "deleteMessage"] ->
              let+ ids = list (field "_id" string) in
              (room_id, Delete_messages ids)
           | _ ->
              fail "Unrecognized stream-notify-room")
       | _ ->
          ksprintf fail "Unrecognized collection %s." collection)
  end

type t = {
  uri: Uri.t;
  receive: unit -> Websocket.Frame.t Lwt.t;
  send: Websocket.Frame.t -> unit Lwt.t;
  call_delay: Ptime.Span.t;
  receivers: (int, (json, error) result Lwt.u) Hashtbl.t;
  room_messages_subscribers:
    (string, room_messages_event -> unit Lwt.t) Hashtbl.t;
  mutable next_call_id: int;
  mutable last_call_time: Ptime.t;
  mutable latest_ping: Ptime.t;
  mutable listener: [`Lost_connection] Lwt.t;
}

let uri c = c.uri

let connect_call_id = -1

module Frame = Websocket.Frame
module Opcode = Websocket.Frame.Opcode

let pp_content ppf content =
  let content =
    if String.length content <= 120 then content else
    String.sub content 0 115 ^ " [...]"
  in
  let is_safe_char = function
   | '\x00'..'\x1f' -> false
   | '\x20'..'\x7e' -> true
   | '\x7f' -> false
   | '\x80'..'\xff' -> true (* Needs Unicode library. *)
  in
  if String.for_all is_safe_char content
  then Format.pp_print_string ppf content
  else Format.fprintf ppf "%S" content

let rec listen conn =
  let* frame = conn.receive () in
  let content = frame.Frame.content in

  let on_ping _json =
    Log.debug (fun f -> f "Received ping as a text frame.") >>= fun () ->
    let content = {|{"msg": "pong"}|} in
    conn.send (Frame.create ~opcode:Opcode.Text ~content ())
  in
  let on_connected json =
    (match Hashtbl.find conn.receivers connect_call_id with
     | exception Not_found ->
        Log.err (fun f -> f "Unexpected connected response.")
     | receiver ->
        Lwt.wakeup_later receiver (Ok json);
        Lwt.return_unit)
  in
  let on_result json =
    (match Decode.decode_value call_response_decoder json with
     | Ok resp ->
        (match Hashtbl.find conn.receivers (int_of_string resp.id) with
         | exception Not_found ->
            Log.err (fun f -> f "No receiver for message id %s." resp.id)
         | exception Failure _ ->
            Log.err (fun f -> f "Bad message id %a." pp_content resp.id)
         | receiver ->
            resp.result_or_error
              |> Result.map_error (fun err -> `Result_error err)
              |> Lwt.wakeup_later receiver;
            Lwt.return_unit)
     | Error msg ->
        Debug.dump_json json >>= fun () ->
        Log.err (fun f ->
          f "Failed to decode result-response %a: %a"
            pp_content content Decode.pp_error msg))
  in
  let on_ready json =
    (match Decode.decode_value ready_decoder json with
     | Ok ids ->
        let wake id =
          (match Hashtbl.find conn.receivers (int_of_string id) with
           | exception Not_found ->
              Log.err (fun f -> f "No receiver for ready-message %s." id)
           | exception Failure _ ->
              Log.err (fun f -> f "Bad ready-message id %s." id)
           | receiver ->
              Lwt.wakeup_later receiver (Ok `Null);
              Lwt.return_unit)
        in
        Lwt_list.iter_s wake ids
     | Error msg ->
        Debug.dump_json json >>= fun () ->
        Log.err (fun f ->
          f "Failed to decode ready-response %a: %a"
            pp_content content Decode.pp_error msg))
  in
  let on_changed json =
    (match Decode.decode_value changed_decoder json with
     | Ok (event_name, room_messages_event) ->
        (match Hashtbl.find conn.room_messages_subscribers event_name with
         | exception Not_found ->
            Log.err (fun f -> f "No receiver for messages in %s." event_name)
         | receiver ->
            receiver room_messages_event)
     | Error msg ->
        Debug.dump_json json >>= fun () ->
        Log.err (fun f ->
          f "Failed to decode changed-response %a: %a"
            pp_content content Decode.pp_error msg))
  in
  let on_error json =
    (match Decode.decode_value operational_error_decoder json with
     | Error _ ->
        Debug.dump_json json >>= fun () ->
        Log.err (fun f -> f "Failed to parse error.")
     | Ok _err ->
        Log.info (fun f -> f "Received unhandled error."))
  in
  (match frame.Frame.opcode with
   | Opcode.Continuation ->
      (* TODO? *)
      Log.warn (fun f -> f "Ignoring continuation message.") >>= fun () ->
      listen conn
   | Opcode.Text ->
      Log.debug (fun f ->
        f "Received frame %a" pp_content content) >>= fun () ->
      (match Yojson.Basic.from_string content with
       | exception (Yojson.Json_error msg) ->
          Log.err (fun f ->
            f "Failed to parse %a as JSON: %s" pp_content content msg)
       | `Assoc alist as json ->
          (match List.assoc "msg" alist with
           | `String "connected" -> on_connected json
           | `String "ping" -> on_ping json
           | `String "error" -> on_error json
           | `String "result" -> on_result json
           | `String "ready" -> on_ready json
           | `String "changed" -> on_changed json
           | _ ->
              Log.err (fun f ->
                f "Ignoring %a due to unknown type." pp_content content)
           | exception Not_found ->
              if List.mem_assoc "server_id" alist then Lwt.return_unit else
              Log.err (fun f ->
                f "Received unknown message %a." pp_content content))
       | _ ->
          Log.err (fun f -> f "Received non-object %a." pp_content content))
      >>= fun () ->
      listen conn
   | Opcode.Binary ->
      Log.warn (fun f -> f "Ignoring binary message.") >>= fun () ->
      listen conn
   | Opcode.Close ->
      Lwt.return `Lost_connection
   | Opcode.Ping ->
      conn.latest_ping <- Ptime_clock.now ();
      Log.info (fun f -> f "Received ping.") >>= fun () ->
      conn.send (Frame.create ~opcode:Opcode.Pong ()) >>= fun () ->
      listen conn
   | Opcode.Pong ->
      Log.info (fun f -> f "Received pong.") >>= fun () ->
      conn.latest_ping <- Ptime_clock.now ();
      listen conn
   | Opcode.Ctrl _ | Opcode.Nonctrl _ ->
      listen conn)

let throttle conn =
  let now = Ptime_clock.now () in
  let age = Ptime.diff now conn.last_call_time in
  let age_left = Ptime.Span.to_float_s (Ptime.Span.sub conn.call_delay age) in
  let+ () =
    if age_left <= 0.0 then Lwt.return_unit else
    let* () = Log.info (fun f -> f "Throttling for %.3g s." age_left) in
    Lwt_unix.sleep age_left
  in
  conn.last_call_time <- now

let call_connect conn =
  let content = {|{"msg": "connect", "version": "1", "support": ["1"]}|} in
  let waiter, resolver = Lwt.task () in
  Hashtbl.add conn.receivers connect_call_id resolver;
  Lwt.finalize
    (fun () ->
      let* () = conn.send (Frame.create ~opcode:Opcode.Text ~content ()) in
      let*? resp =
        waiter >|= Result.map_error (fun err -> (err : error :> [> error]))
      in
      (match Decode.decode_value connected_decoder resp with
       | Error _ ->
          Log.err (fun f ->
            f "Bad connected-message %a." pp_content content) >|= fun () ->
          Error (`Protocol_error "Bad connected message.")
       | Ok connected ->
          Log.info (fun f ->
            f "Connected with session %s." connected.session) >|= fun () ->
          Ok ()))
    (fun () ->
      Hashtbl.remove conn.receivers connect_call_id;
      Lwt.return_unit)

let send_and_receive conn id req =
  let* () = throttle conn in
  let waiter, resolver = Lwt.task () in
  Hashtbl.add conn.receivers id resolver;
  Lwt.finalize
    (fun () ->
      let* () = conn.send (Frame.create ~opcode:Opcode.Text ~content:req ()) in
      waiter)
    (fun () -> Hashtbl.remove conn.receivers id; Lwt.return_unit)

let call_json conn method_ params =
  let call_id = conn.next_call_id in
  conn.next_call_id <- conn.next_call_id + 1;
  let req =
    let open Encode in
    Yojson.Basic.to_string @@ obj [
      "msg", string "method";
      "id", string (string_of_int call_id);
      "method", string method_;
      "params", list value params;
    ]
  in
  Log.debug (fun f -> f "Call #%d: << %s" call_id req) >>= fun () ->
  let*? resp = send_and_receive conn call_id req in
  Log.debug (fun f -> f "Call #%d: >> %a" call_id pp_json_briefly resp)
    >|= fun () ->
  Ok resp

let call ~decoder conn method_ params =
  call_json conn method_ params >>=
  (function
   | Ok json ->
      (match Decode.decode_value decoder json with
       | Ok _ as r -> Lwt.return  r
       | Error msg ->
          Debug.dump_json json >|= fun () ->
          Error (`Cannot_decode (msg, json)))
   | Error err -> Lwt.return_error (err : error :> [> error]))

let subscribe ~name ~params conn =
  let sub_id = conn.next_call_id in
  conn.next_call_id <- conn.next_call_id + 1;
  let req = Yojson.Basic.to_string @@ Encode.obj [
    "msg", Encode.string "sub";
    "id", Encode.string (string_of_int sub_id);
    "name", Encode.string name;
    "params", Encode.list Fun.id params;
  ] in
  Log.debug (fun f -> f "Subscription #%d %s." sub_id req) >>= fun () ->
  let+? resp = send_and_receive conn sub_id req in
  assert (resp = `Null)

let subscribe_to_room_messages ~on_event ~room conn =
  let (^/) = Filename.concat in
  let room_id = Room.id room in
  let room_name = Room.name room in
  let* () = Log.info (fun f -> f "Subscribing to %s (%s)." room_name room_id) in
  Hashtbl.add conn.room_messages_subscribers room_id on_event;
  begin
    subscribe conn
      ~name:"stream-room-messages"
      ~params:[`String room_id; `Bool false]
      >>=? fun () ->
    subscribe conn
      ~name:"stream-notify-room"
      ~params:[`String (room_id ^/ "deleteMessage"); `Bool false]
  end >|= Result.map_error (fun err -> (err : error :> [> error]))

let gethostbyname ~dns_client host =
  let host' = host |> Domain_name.of_string_exn |> Domain_name.host_exn in
  let* ip4 = Dns_client_lwt.gethostbyname dns_client host' in
  (match ip4 with
   | Ok ip4 -> Lwt.return_ok (Ipaddr.V4 ip4)
   | Error _ ->
      let+? ip6 = Dns_client_lwt.gethostbyname6 dns_client host' in
      Ipaddr.V6 ip6)

let connect ~dns_client ?(call_delay = Ptime.Span.of_int_s 5) uri =
  let connect_to endp =
    let+ receive, send = Websocket_lwt_unix.with_connection endp uri in
    {
      uri; receive; send; call_delay;
      receivers = Hashtbl.create 11;
      room_messages_subscribers = Hashtbl.create 11;
      next_call_id = 0;
      last_call_time = Ptime.min;
      latest_ping = Ptime.min;
      listener = Lwt.return `Lost_connection;
    }
  in
  (match Uri.scheme uri, Uri.host uri with
   | Some "wss", Some host ->
      let*? ip = gethostbyname ~dns_client host in
      let port = Option.get_or 443 (Uri.port uri) in
      let* conn = connect_to (`TLS (`Hostname host, `IP ip, `Port port)) in
      conn.listener <- listen conn;
      let+? () = call_connect conn in
      conn
   | _ ->
      Lwt.return_error (`Msg ("Unexpected WebSocket URI " ^ Uri.to_string uri)))