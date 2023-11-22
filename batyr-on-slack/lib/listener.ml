(* Copyright (C) 2018--2023  Petter A. Urkedal <paurkedal@gmail.com>
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
open Unprime_list
open Unprime_option
open Unprime_string

let pp_ptime = Ptime.pp_human ~frac_s:6 ()

(*
let ptime_span_of_string s =
  let s = String.trim s in
  let i = String.rskip_while Char.is_alnum s (String.length s) in
  let x = float_of_string String.(slice 0 i s |> trim) in
  let scale =
    (match String.slice_from i s with
     | "ms" | "millisecond" -> 1e-3
     | "" | "s" | "second" -> 1.0
     | "min" | "minute" -> 60.0
     | "h" | "hour" -> 3600.0
     | "day" -> 86400.0
     | _ -> failwith "Invalid time unit.") in
  (match Ptime.Span.of_float_s (x *. scale) with
   | Some t -> t
   | None -> failwith "Invalid time span.")
*)

let require what = function
 | None -> Error (`Msg ("Did not find " ^ what ^ "."))
 | Some x -> Ok x

module Config = Config

let log_level_of_string s =
  (match Logs.level_of_string s with
   | Ok level -> level
   | Error (`Msg msg) -> failwith msg)

let (>>=?) m f =
  m >>= function
   | Ok x -> f x
   | Error error -> Lwt.return_error error

module Req = struct
  include Caqti_request.Infix
  include Caqti_type.Std
end

module Make (Batyr_data : Batyr_core.Data_sig.S) = struct
  open Batyr_data

  type monitor_state = {
    cache: Slack_cache.t;
    team_info: Slack_rtm.team_info;
    conference_domain: string;
    recipient: Resource.t;
  }

  let find_message_id =
    let q =
      Req.(t3 int ptime int ->! int32)
      "SELECT message_id FROM batyr.messages \
       WHERE recipient_id = ? AND seen_time = ? AND sender_id = ?" in
    fun recipient seen_time sender ->
      (Resource.stored_id recipient >|= require "recipient")
        >>=? fun recipient_id ->
      (Resource.stored_id sender >|= require "sender")
        >>=? fun sender_id ->
      Log.debug (fun m ->
        m "Locating message from #%d to #%d at %a."
          sender_id recipient_id pp_ptime seen_time) >>= fun () ->
      Db.use
        (fun (module C) -> C.find q (recipient_id, seen_time, sender_id))

  let delete_message_id =
    let q =
      Req.(int32 ->. unit)
      "DELETE FROM batyr.messages WHERE message_id = ?" in
    fun message_id -> Db.use (fun (module C) -> C.exec q message_id)

  let update_message_id =
    let q =
      Req.(t4 ptime int string int32 ->. unit)
      "UPDATE batyr.messages SET edit_time = ?, sender_id = ?, body = ? \
       WHERE message_id = ?" in
    fun message_id ~new_ts ~new_subtype ~new_sender ~new_text () ->
      (Resource.stored_id new_sender >|= require "sender")
        >>=? fun new_sender_id ->
      let body =
        (match new_subtype with
         | None | Some "message" -> Ok new_text
         | Some "me_message" -> Ok ("/me " ^ new_text)
         | _ -> Error (`Msg "Unhandled subtype for new message.")) in
      (match body with
       | Ok body ->
          Db.use
            (fun (module C) -> C.exec q (new_ts, new_sender_id, body, message_id))
       | Error err -> Lwt.return_error err)

  let store_message {cache; recipient; _}
      ~channel_node ~user ~subtype ?text ~ts () =
    Slack_cache.user_obj_of_user cache user >>=? fun user_obj ->
    let user_name = user_obj.Slacko.name in
    let sender = Resource.create_on_node channel_node user_name in
    let store body =
      let message_type = `Groupchat in
      Message.store @@
        Message.make ~seen_time:ts ~sender ~recipient ~message_type ~body ()
    in
    (match subtype, text with
     | None, Some text ->
        Log.debug (fun m -> m "Storing message.") >>= fun () ->
        let%lwt body = Slack_utils.demarkup cache text in
        store body
     | Some "me_message", Some text ->
        Log.debug (fun m -> m "Storing /me message.") >>= fun () ->
        let%lwt body = Slack_utils.demarkup cache text in
        store ("/me " ^ body)
     | Some t, _ ->
        Log.info (fun m -> m "Ignoring message of type %s." t) >>= fun () ->
        Lwt.return_ok ()
     | _, None ->
        Log.info (fun m -> m "Ignoring empty message.") >>= fun () ->
        Lwt.return_ok ())

  let update_message {cache; recipient; _}
      ~channel_node ~old_ts ~old_user ~new_ts ~new_user ~new_subtype ~new_text
      () =
    Log.debug (fun m -> m "Updating message.") >>= fun () ->

    Slack_cache.user_obj_of_user cache old_user >>=? fun old_user_obj ->
    let old_user_name = old_user_obj.Slacko.name in
    let old_sender = Resource.create_on_node channel_node old_user_name in

    Slack_cache.user_obj_of_user cache new_user >>=? fun new_user_obj ->
    let new_user_name = new_user_obj.Slacko.name in
    let new_sender = Resource.create_on_node channel_node new_user_name in

    find_message_id recipient old_ts old_sender >>=? fun message_id ->
    Log.debug (fun m -> m "Updating message #%ld." message_id) >>= fun () ->
    update_message_id message_id ~new_ts ~new_subtype ~new_sender ~new_text ()

  let delete_message {cache; recipient; _}
      ~channel_node ~old_ts ~old_user () =

    Slack_cache.user_obj_of_user cache old_user >>=? fun old_user_obj ->
    let old_user_name = old_user_obj.Slacko.name in
    let old_sender = Resource.create_on_node channel_node old_user_name in

    find_message_id recipient old_ts old_sender >>=? fun message_id ->
    Log.debug (fun m -> m "Deleting message #%ld." message_id) >>= fun () ->
    delete_message_id message_id

  let store_rtm_message state message_event =
    let open Slack_rtm in
    let channel =
      Slacko.channel_of_string (string_of_channel message_event.channel) in
    Slack_cache.channel_obj_of_channel state.cache channel
      >>=? fun channel_obj ->
    let channel_name = channel_obj.Slacko.name in
    let channel_node =
      Node.create
        ~domain_name:state.conference_domain
        ~node_name:channel_name () in
    (match%lwt Muc_room.stored_of_node channel_node with
     | None ->
        Log.debug (fun m ->
          m "No record of %s." (Node.to_string channel_node)) >>= fun () ->
        Lwt.return_ok ()
     | Some muc_room when not (Muc_room.transcribe muc_room) ->
        Log.debug (fun m -> m "Ignoring message for room.") >>= fun () ->
        Lwt.return_ok ()
     | Some _ ->
        (match message_event.sub with
         | `Add msg ->
            store_message state
              ~channel_node
              ~ts:message_event.ts
              ~user:(Slacko.user_of_string (string_of_user msg.user))
              ~subtype:msg.subtype
              ~text:msg.text
              ()
         | `Change msg ->
            update_message state
              ~channel_node
              ~old_ts:msg.previous_message.ts
              ~old_user:
                (Slacko.user_of_string (string_of_user msg.previous_message.user))
              ~new_ts:msg.message.edited_ts
              ~new_user:(Slacko.user_of_string (string_of_user msg.message.user))
              ~new_subtype:msg.message.subtype
              ~new_text:msg.message.text
              ()
         | `Delete msg ->
            delete_message state
              ~channel_node
              ~old_ts:msg.previous_message.ts
              ~old_user:
                (Slacko.user_of_string (string_of_user msg.previous_message.user))
            ()
         | `Other json ->
            Log.debug (fun m ->
              m "Unhandled event %s" (Yojson.Basic.to_string json)) >|= fun () ->
            Ok ()))

  let store_slacko_message state ~channel (message_obj : Slacko.message_obj) =
    let open Slacko in
    (match%lwt
      Slack_cache.channel_obj_of_channel state.cache channel
        >>=? fun channel_obj ->
      let channel_name = channel_obj.Slacko.name in
      let channel_node =
        Node.create
          ~domain_name:state.conference_domain
          ~node_name:channel_name () in
      (match message_obj.user with
       | Some user ->
          store_message state
            ~channel_node
            ~user
            ~subtype:None
            ?text:message_obj.text
            ~ts:message_obj.ts ()
       | None ->
          Lwt.return_error (`Msg "No user present in historic message."))
     with
     | Ok () -> Lwt.return_unit
     | Error (`Msg msg) ->
        Log.err (fun m ->
          m "Failed to store message: %s" msg)
     | Error (#Slack_utils.showable_error as err) ->
        Log.err (fun m ->
          m "Failed to store message: %a" Slack_utils.pp_error err)
     | Error (#Caqti_error.t as err) ->
        Log.err (fun m ->
          m "Failed to store message: %a" Caqti_error.pp err))

  let messages_ts_range message_objs =
    let aux (message_obj : Slacko.message_obj) (ts_min, ts_max) =
      let ts = message_obj.Slacko.ts in
      ((if Ptime.is_earlier ~than:ts_min ts then ts else ts_min),
       (if Ptime.is_later ~than:ts_max ts then ts else ts_max)) in
    List.fold aux message_objs (Ptime.max, Ptime.min)

  let rec fetch_recent state channelname oldest =
    let session = Slack_cache.session state.cache in
    let channel = Slacko.channel_of_string ("#" ^ channelname) in
    (match%lwt Slacko.channels_history session ~oldest ~inclusive:false channel
     with
     | `Success history_obj ->
        let messages = history_obj.Slacko.messages in
        if messages = [] then
          Log.info (fun m ->
            m "Received no past messages for %s." channelname)
        else begin
          let ts_min, ts_max = messages_ts_range messages in
          Log.info (fun m ->
            m "Received %d messages for %s in time range [%a, %a]."
              (List.length messages) channelname
              pp_ptime ts_min pp_ptime ts_max)
            >>= fun () ->
          (* TODO: Sort messages on date and abort on first failure. *)
          Lwt_list.iter_s (store_slacko_message state ~channel) messages
            >>= fun () ->
          if history_obj.has_more then
            let latest =
              List.fold
                Slacko.(fun (message_obj : message_obj) -> max message_obj.ts)
                messages oldest in
            fetch_recent state channelname latest
          else
            Lwt.return_unit
        end
     | #Slacko.parsed_auth_error
     | #Slacko.channel_error
     | #Slacko.timestamp_error as err ->
        Log.err (fun m -> m "Failed to fetch history for %s: %s"
          channelname (Slack_utils.show_error err)))

  let transcribed_rooms_q =
    Req.(string ->* t2 string (option ptime))
    {|
      SELECT
        room_node.node_name,
        ( SELECT max(seen_time) FROM batyr.messages msg
          JOIN batyr.resources sender ON sender.resource_id = msg.sender_id
          WHERE sender.node_id = room_node.node_id )
      FROM batyr.muc_rooms AS room
      JOIN batyr.nodes   AS room_node ON room_node.node_id  = room.node_id
      JOIN batyr.domains AS room_dom  ON room_dom.domain_id = room_node.domain_id
      WHERE room.transcribe = true AND room_dom.domain_name = ?
    |}

  let fetch_all_recent state =
    Db.use_exn
      (fun (module C) ->
        C.collect_list transcribed_rooms_q state.conference_domain)
    >>= Lwt_list.iter_s
      (function
       | channelname, None ->
          Log.info
            (fun m -> m "Not fetching history for new channel %s." channelname)
       | channelname, Some latest ->
          Log.info (fun m ->
            m "Requesting messages since %a for %s."
              pp_ptime latest channelname) >>= fun () ->
          fetch_recent state channelname latest)

  let rec monitor state conn =
    (match%lwt Slack_rtm.receive conn with
     | Ok message_event ->
        (match%lwt store_rtm_message state message_event with
         | Ok () -> Lwt.return_unit
         | Error (#Slack_cache.error as error) ->
            Log.err (fun m ->
              m "Failed to capture message event: %a" Slack_utils.pp_error error)
         | Error (#Caqti_error.t as error) ->
            Log.err (fun m ->
              m "Failed to update database: %a" Caqti_error.pp error)
         | Error (`Msg msg) ->
            Log.err (fun m ->
              m "Failed to assimilate message event: %s" msg))
        >>= fun () ->
        monitor state conn
     | Error `Closed ->
        Log.info (fun m -> m "Connection closed.")
     | Error (`Msg msg) ->
        Log.err (fun m -> m "%s" msg) >>= fun () ->
        monitor state conn)
end

let launch config =

  let dns_client =
    let nameservers =
      let open Config in
      let aux cfg = `Plaintext (Ipaddr.of_string_exn cfg.host, cfg.port) in
      (`Tcp, (List.map aux config.nameservers))
    in
    Dns_client_lwt.create ~nameservers ()
  in

  let cache = Slack_cache.create ~token:config.slack_token () in
  let bot_token = Option.get_or config.slack_token config.slack_bot_token in

  (match%lwt Slack_rtm.connect ~dns_client ~token:bot_token () with
   | Ok conn ->
      let storage_uri = Uri.of_string config.storage_uri in
      let module B = (val Batyr_core.Data.connect storage_uri) in
      let module L = Make (B) in
      let disconnect_status = ref `Lost_connection in
      let disconnected, disconnect = Lwt.wait () in
      let kill_handler sn =
        disconnect_status := `Signalled sn;
        Lwt.wakeup_later disconnect () in
      let kill_handler_ids =
        List.map (fun sn -> Lwt_unix.on_signal sn kill_handler) [1; 2; 15] in
      let team_info = Slack_rtm.team_info conn in
      let team_name = team_info.Slack_rtm.name in
      let user_info = Slack_rtm.user_info conn in
      let user_name = user_info.Slack_rtm.name in
      let conference_domain =
        sprintf "conference.%s.xmpp.slack.com"
          (String.lowercase_ascii team_name) in
      let recipient =
        let domain_name = team_name ^ ".xmpp.slack.com" in
        let resource_name = "batyr-logger-slack" in
        B.Resource.create ~domain_name ~node_name:user_name ~resource_name () in
      let state = L.{cache; team_info; conference_domain; recipient} in
      let recent_fetcher = L.fetch_all_recent state in
      Lwt.join [
        recent_fetcher;
        Lwt.choose [L.monitor state conn; disconnected]
      ] >>= fun () ->
      List.iter Lwt_unix.disable_signal_handler kill_handler_ids;
      Log.info (fun m -> m "Disconnecting and exiting.") >>= fun () ->
      Slack_rtm.disconnect conn >|= fun () ->
      !disconnect_status
   | Error (`Msg s) ->
      Log.err (fun m -> m "%s" s) >|= fun () ->
      `Failed_to_connect)
