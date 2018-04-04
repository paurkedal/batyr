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
  sender_domain: string;
  recipient: Resource.t;
}

let store_message {cache; team_info; sender_domain; recipient} message =
  let open Slack_rtm in
  let channel_id = string_of_channel message.channel in
  Slack_cache.channel_obj_of_id cache channel_id >>=? fun channel_obj ->
  let user_id = string_of_user message.user in
  Slack_cache.user_obj_of_id cache user_id >>=? fun user_obj ->
  let channel_name = channel_obj.Slacko.name in
  let user_name = user_obj.Slacko.name in
  let sender =
    Resource.create
      ~domain_name:sender_domain ~node_name:channel_name
      ~resource_name:user_name () in
  (match message.subtype with
   | None ->
      Message.store @@ Message.make
        ~seen_time:message.ts
        ~sender
        ~recipient
        ~message_type:Batyr_xmpp.Chat.Groupchat (* FIXME *)
        ~body:message.text ()
   | Some t ->
      Logs_lwt.info (fun m -> m "Ignoring message of type %s." t)) >>= fun () ->
  Lwt.return_ok ()

let rec monitor state conn =
  (match%lwt Slack_rtm.receive conn with
   | Ok (`Message message) ->
      Logs_lwt.debug Slack_rtm.(fun m ->
        m "message from %s: %s" (string_of_user message.user) message.text)
        >>= fun () ->
      (match%lwt store_message state message with
       | Ok () -> Lwt.return_unit
       | Error error ->
          (* TODO: Report properly. *)
          Logs_lwt.err (fun m -> m "Failed to store message.")) >>= fun () ->
      monitor state conn
   | Error `Closed ->
      Logs_lwt.info (fun m -> m "Connection closed.") >>= fun () ->
      Lwt.return 0
   | Error (`Msg msg) ->
      Logs_lwt.err (fun m -> m "%s" msg) >>= fun () ->
      Lwt.return 69)

let main config_path =
  let config = load_config config_path in
  Logs.set_level config.log_level;
  let cache = Slack_cache.create ~token:config.token () in
  Lwt_main.run
    (match%lwt Slack_rtm.connect ~token:config.token () with
     | Ok conn ->
        let team_info = Slack_rtm.team_info conn in
        let team_name = team_info.Slack_rtm.name in
        let user_info = Slack_rtm.user_info conn in
        let user_name = user_info.Slack_rtm.name in
        let sender_domain = sprintf "conference.%s.xmpp.slack.com" team_name in
        let recipient =
          let domain_name = team_name ^ ".xmpp.slack.com" in
          let resource_name = "batyr-logger-slack" in
          Resource.create ~domain_name ~node_name:user_name ~resource_name () in
        monitor {cache; team_info; sender_domain; recipient} conn
     | Error (`Msg s) -> Logs_lwt.err (fun m -> m "%s" s) >|= fun () -> 69)

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
