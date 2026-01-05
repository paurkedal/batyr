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

open Batyr_core.Prereq
open Lwt.Infix
open Lwt.Syntax
open Unprime_list
module R = Rockettime

module Config = Config

let fetch_rooms config conn =
  let*? resp =
    R.Methods.get_rooms conn >|= Result.map_error @@ fun err ->
    `Msg (Fmt.str "Cannot load rooms: %a" R.Connection.pp_error err)
  in
  let rooms =
    (match config.Config.include_rooms with
     | None -> resp.update
     | Some include_rooms ->
        let is_included = function
         | R.Room.Chat_room room ->
            List.mem room.R.Room.name include_rooms
         | Private_room room ->
            List.mem room.R.Room.name include_rooms
         | Direct_chat_room _ | Livechat_room _ -> false
        in
        List.filter is_included resp.update)
  in
  Log.debug (fun f ->
    f "Found %d rooms to update and %d rooms to remove."
      (List.length resp.update) (List.length resp.remove)) >|= fun () ->
  Ok rooms

let messages_ts_range messages =
  let open R.Message in
  let aux m (ts_min, ts_max) =
    let ts_min = if Ptime.compare m.ts ts_min < 0 then m.ts else ts_min in
    let ts_max = if Ptime.compare m.ts ts_max > 0 then m.ts else ts_max in
    (ts_min, ts_max)
  in
  List.fold aux messages (Ptime.max, Ptime.min)

let pp_message_id = Fmt.(using (fun {R.Message.id; _} -> id) string)
let pp_message_ids = Fmt.(list ~sep:comma pp_message_id)
let pp_strings = Fmt.(list ~sep:comma string)

module Make_listener (B : Batyr_core.Data_sig.S) = struct
  module I = Injector.Make (B)

  let room_recipient conn room =
    let domain_name = Uri.host (R.Connection.uri conn) |> Option.get in
    let node_name = R.Room.name room in
    let foreign_resource_id = R.Room.id room in
    B.Resource.create ~domain_name ~node_name ~foreign_resource_id ()

  let subscribe_to_room conn room =
    let recipient = room_recipient conn room in
    let on_event : R.Connection.room_messages_event -> _ = function
     | Update_messages messages ->
        Lwt_result_list.iter_s (I.store_message ~recipient) messages >>=
        (function
         | Ok () ->
            Log.debug (fun f ->
              f "Updated %a" pp_message_ids messages)
         | Error err ->
            Log.err (fun f ->
              f "Failed to update %a: %a" pp_message_ids messages
                Caqti_error.pp err))
     | Delete_messages message_ids ->
        Lwt_result_list.iter_s (I.delete_message ~recipient) message_ids >>=
        (function
         | Ok () ->
            Log.debug (fun f ->
              f "Deleted %a" pp_strings message_ids)
         | Error err ->
            Log.err (fun f ->
              f "Failed to delete %a: %a" pp_strings message_ids
                Caqti_error.pp err))
    in
    R.Connection.subscribe_to_room_messages ~on_event ~room conn

  let load_missed_in_room ~config conn room =
    if not config.Config.backlog_enabled then Lwt.return_ok () else
    let recipient = room_recipient conn room in
    let room_name = R.Room.name room in
    Log.debug (fun f -> f "Loading missed messages in %s." room_name)
      >>= fun () ->
    let room_id = R.Room.id room in
    let*? latest = I.latest_timestamp ~recipient () in
    let since =
      (* Extend the backlog if requested by the configuration, in order to catch
       * edits of older messages, since the current API does not support
       * filtering messages on the time of the last change. *)
      Ptime.sub_span latest config.Config.backlog_extension_period
        |> Option.value ~default:latest
    in
    let*? history =
      R.Methods.load_history ~room_id ~since conn
        >|= Result.map_error @@ fun err ->
      `Msg (Fmt.str "Cannot load history: %a" R.Connection.pp_error err)
    in
    Log.info (fun f ->
      let ts_min, ts_max = messages_ts_range history.messages in
      f "Received %d missed messages from %a to %a, excluding %d, from %s"
        (List.length history.messages) Ptime.pp ts_min Ptime.pp ts_max
        history.unread_not_loaded room_name) >>= fun () ->
    Lwt_result_list.iter_s (I.store_message ~recipient)
      history.messages

  let launch_room ~config conn room =
    I.enable_room ~recipient:(room_recipient conn room) () >>=? fun () ->
    let*? () =
      if not config.Config.listen_enabled then Lwt.return_ok () else
      subscribe_to_room conn room
    in
    load_missed_in_room ~config conn room

end

let pp_error ppf = function
 | #R.Connection.error as err -> R.Connection.pp_error ppf err
 | #Caqti_error.t as err -> Caqti_error.pp ppf err

let launch config =

  let dns_client =
    let nameservers =
      let open Config in
      let aux cfg = `Plaintext (Ipaddr.of_string_exn cfg.host, cfg.port) in
      (`Tcp, (List.map aux config.nameservers))
    in
    let happy_eyeballs = Happy_eyeballs_lwt.create () in
    Dns_client_lwt.create ~nameservers happy_eyeballs
  in
  let rocketchat_uri = Uri.of_string config.rocketchat_uri in

  Log.debug (fun f -> f "Conneting to %s" config.rocketchat_uri) >>= fun () ->
  begin
    let storage_uri = Uri.of_string config.Config.storage_uri in
    let module B = (val Batyr_core.Data.connect storage_uri) in
    let module L = Make_listener (B) in
    let*? conn = R.Connection.connect ~dns_client rocketchat_uri in
    let*? _ = R.Methods.resume_with_token ~token:config.rocketchat_token conn in
    let*? rooms = fetch_rooms config conn in
    let*? () = Lwt_result_list.iter_s (L.launch_room ~config conn) rooms in
    if config.Config.listen_enabled then
      R.Connection.wait conn >|= fun () -> Result.ok `Lost_connection
    else
      R.Connection.close conn >|= fun () -> Result.ok (`Exit 0)
  end >>= function
   | Ok st -> Lwt.return st
   | Error (`Msg s) ->
      let+ () = Log.err (fun f -> f "%s" s) in
      `Exit 69
   | Error err ->
      let+ () = Log.err (fun f -> f "%a" pp_error err) in
      `Failed_to_connect
