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

open Batyr_data
open Lwt.Infix
open Printf
open Unprime_list
open Unprime_option

type config = {
  token: string;
  log_level: Logs.level option;
}

let log_level_of_string s =
  (match Logs.level_of_string s with
   | Ok level -> level
   | Error (`Msg msg) -> failwith msg)

let load_config path =
  let open Kojson_pattern in
  let json = Yojson.Basic.from_file path in
  Kojson.jin_of_json json |> K.assoc begin
    "token"^: K.string %> fun token ->
    "log-level"^?:
      Option.fmap (K.convert_string "Logs.level" log_level_of_string)
        %> fun log_level ->
    Ka.stop {token; log_level}
  end

let (>>=?) m f =
  m >>= function
   | Ok x -> f x
   | Error error -> Lwt.return_error error

type monitor_state = {
  cache: Slack_cache.t;
  team_info: Slack_rtm.team_info;
  conference_domain: string;
  recipient: Resource.t;
}

let store_message {cache; team_info; conference_domain; recipient}
    ~channel ~user ~subtype ~text ~ts () =
  Slack_cache.channel_obj_of_channel cache channel >>=? fun channel_obj ->
  Slack_cache.user_obj_of_user cache user >>=? fun user_obj ->
  let channel_name = channel_obj.Slacko.name in
  let user_name = user_obj.Slacko.name in
  let channel_node =
    Node.create ~domain_name:conference_domain ~node_name:channel_name () in
  let sender = Resource.create_on_node channel_node user_name in
  (match%lwt Muc_room.stored_of_node channel_node with
   | None ->
      Logs_lwt.debug (fun m ->
        m "No record of %s." (Node.to_string channel_node)) >>= fun () ->
      Lwt.return_ok ()
   | Some muc_room when not (Muc_room.transcribe muc_room) ->
      Logs_lwt.debug (fun m -> m "Ignoring message for room.") >>= fun () ->
      Lwt.return_ok ()
   | Some muc_room ->
      (match subtype with
       | None ->
          Logs_lwt.debug (fun m -> m "Storing message.") >>= fun () ->
          let%lwt body = Slack_utils.demarkup cache text in
          Message.store @@ Message.make
            ~seen_time:ts
            ~sender
            ~recipient
            ~message_type:`Groupchat (* FIXME *)
            ~body
            ()
       | Some t ->
          Logs_lwt.info (fun m -> m "Ignoring message of type %s." t))
      >>= Lwt.return_ok)

let store_rtm_message state message =
  let open Slack_rtm in
  store_message state
    ~channel:(Slacko.channel_of_string (string_of_channel message.channel))
    ~user:(Slacko.user_of_string (string_of_user message.user))
    ~subtype:message.subtype
    ~text:message.text
    ~ts:message.ts
    ()

let store_slacko_message state ~channel (message_obj : Slacko.message_obj) =
  let open Slacko in
  (match Ptime.of_float_s message_obj.ts with
   | Some ts ->
      (match%lwt
        store_message state
          ~channel
          ~user:message_obj.user
          ~subtype:None
          ~text:message_obj.text
          ~ts ()
       with
       | Ok () -> Lwt.return_unit
       | Error err ->
          Logs_lwt.err (fun m -> m "Failed to store message"))
   | None ->
      Logs_lwt.err (fun m -> m "Invalid time float %g." message_obj.ts))

let rec fetch_recent state channelname oldest =
  let session = Slack_cache.session state.cache in
  let channel = Slacko.channel_of_string ("#" ^ channelname) in
  (match%lwt Slacko.channels_history session ~oldest ~inclusive:false channel
   with
   | `Success history_obj ->
      let messages = history_obj.Slacko.messages in
      Logs_lwt.info (fun m ->
        m "Received %d past messages for %s."
          (List.length messages) channelname) >>= fun () ->
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
   | #Slacko.parsed_auth_error
   | #Slacko.channel_error
   | #Slacko.timestamp_error as err ->
      (* FIXME: Decode error. *)
      Logs_lwt.err (fun m -> m "Failed to fetch history for %s: %s"
        channelname (Slack_utils.show_error err)))

let transcribed_rooms_q = Caqti_request.collect
  Caqti_type.string Caqti_type.(tup2 string (option ptime))
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
  Batyr_db.use_exn
    (fun (module C) ->
      C.collect_list transcribed_rooms_q state.conference_domain)
  >>= Lwt_list.iter_s
    (function
     | channelname, None ->
        Logs_lwt.info
          (fun m -> m "Not fetching history for new channel %s." channelname)
     | channelname, Some latest ->
        fetch_recent state channelname (Ptime.to_float_s latest))

let rec monitor state conn =
  (match%lwt Slack_rtm.receive conn with
   | Ok (`Message message) ->
      Logs_lwt.debug Slack_rtm.(fun m ->
        m "message from %s: %s" (string_of_user message.user) message.text)
        >>= fun () ->
      (match%lwt store_rtm_message state message with
       | Ok () -> Lwt.return_unit
       | Error error ->
          (* TODO: Report properly. *)
          Logs_lwt.err (fun m -> m "Failed to store message.")) >>= fun () ->
      monitor state conn
   | Error `Closed ->
      Logs_lwt.info (fun m -> m "Connection closed.")
   | Error (`Msg msg) ->
      Logs_lwt.err (fun m -> m "%s" msg) >>= fun () ->
      monitor state conn)

let main config_path =
  let config = load_config config_path in
  Logs.set_level config.log_level;
  let cache = Slack_cache.create ~token:config.token () in
  Lwt_main.run
    (match%lwt Slack_rtm.connect ~token:config.token () with
     | Ok conn ->
        let disconnected, disconnect = Lwt.wait () in
        let kill_handler = Lwt_unix.on_signal 2
          (fun _ -> Lwt.wakeup_later disconnect ()) in
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
          Resource.create ~domain_name ~node_name:user_name ~resource_name () in
        let state = {cache; team_info; conference_domain; recipient} in
        let recent_fetcher = fetch_all_recent state in
        fetch_all_recent state >>= fun () ->
        Lwt.join [
          recent_fetcher;
          Lwt.choose [monitor state conn; disconnected]
        ] >>= fun () ->
        Lwt_unix.disable_signal_handler kill_handler;
        Logs_lwt.info (fun m -> m "Disconnecting and exiting.") >>= fun () ->
        Slack_rtm.disconnect conn >|= fun () ->
        0 (* exit code *)
     | Error (`Msg s) ->
        Logs_lwt.err (fun m -> m "%s" s) >|= fun () ->
        69 (* exit code *))

let main_cmd =
  let open Cmdliner in
  let config =
    Arg.(required (pos 0 (some string) None (info ~docv:"CONFIG" []))) in
  let term = Term.(const main $ config) in
  let info = Term.info (Filename.basename Sys.argv.(0)) in
  (term, info)

let () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
       | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
       | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ()) in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k () in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf in
  Logs.set_reporter {Logs.report = report}

let () =
  (match Cmdliner.Term.eval main_cmd with
   | `Error _ -> exit 64
   | `Ok ec -> exit ec
   | `Version | `Help -> exit 0)
