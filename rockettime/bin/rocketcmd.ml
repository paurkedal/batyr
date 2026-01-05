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

open Batyr_core
open Batyr_core.Prereq
open Lwt.Infix
open Lwt.Syntax
module R = Rockettime

module Log = (val Logs_lwt.src_log (Logs.Src.create "rocketcmd"))

module Duration = struct
  type t = Ptime.Span.t

  let of_yojson = function
   | `Int t_s -> Ok (Ptime.Span.of_int_s t_s)
   | `Float t_s ->
      (match Ptime.Span.of_float_s t_s with
       | None -> Error "duration out of range"
       | Some t -> Ok t)
   | _ ->
      Error "expecting float time"

  let to_yojson t = `Float (Ptime.Span.to_float_s t)
end

type nameserver = {
  host: string;
  port: int;
}
[@@deriving yojson]

type config = {
  log_level: Logging.Verbosity.t [@default Logging.Verbosity.default];
  rocketchat_uri: string;
  rocketchat_user: string option [@default None];
  rocketchat_token: string;
  rocketchat_ping_period: Duration.t [@default Ptime.Span.v (240, 0L)];
  rocketchat_ping_patience: Duration.t [@default Ptime.Span.v (600, 0L)];
  include_rooms: string list [@default []];
  nameservers: nameserver list [@default [{host = "127.0.0.53"; port = 53}]];
}
[@@deriving yojson {strict = false}]

let load_config config_path =
  let* config_string =
    Lwt_io.with_file ~mode:Lwt_io.input config_path Lwt_io.read
  in
  let* config_json = Lwt.wrap1 Yojson.Safe.from_string config_string in
  (match config_of_yojson config_json with
   | Ok config -> Lwt.return_ok config
   | Error msg -> Lwt.return_error (`Msg msg))

let wrap_main f config_path =
  Logs.set_reporter (Batyr_core.Logging.lwt_reporter ());
  let report_error = function
   | Ok () -> Lwt.return 0
   | Error err ->
      Log.err (fun f -> f "%a" R.Connection.pp_error err)
        >|= fun () -> 69
  in
  Lwt_main.run (begin
    let*? config = load_config config_path in
    Logging.setup ~verbosity:config.log_level ();
    let* () = Log.debug (fun f -> f "Conneting to %s" config.rocketchat_uri) in
    let dns_client =
      let nameservers =
        let aux cfg = `Plaintext (Ipaddr.of_string_exn cfg.host, cfg.port) in
        (`Tcp, (List.map aux config.nameservers))
      in
      let happy_eyeballs = Happy_eyeballs_lwt.create () in
      Dns_client_lwt.create ~nameservers happy_eyeballs
    in
    let*? conn =
      R.Connection.connect ~dns_client (Uri.of_string config.rocketchat_uri)
    in
    f config conn
  end >>= report_error)

let config_arg =
  Cmdliner.Arg.(required (pos 0 (some string) None (info ~docv:"CONFIG" [])))

(* Login *)

let login = wrap_main begin fun config conn ->
  let*? {id; token; token_expires; _(*TODO:check*)} =
    (match config.rocketchat_user with
     | Some username ->
        R.Methods.login_with_password
          ~username ~password:config.rocketchat_token conn
     | None ->
        R.Methods.resume_with_token
          ~token:config.rocketchat_token conn)
  in
  Log.debug (fun f ->
    f "Logged in as %s with token %s which expires %a."
      id token (Fmt.option Ptime.pp) token_expires) >|= fun () ->
  Ok ()
end

let login_cmd =
  let open Cmdliner in
  let term = Term.(const login $ config_arg) in
  Cmd.v (Cmd.info "login") term

(* Get Rooms *)

let get_rooms = wrap_main begin fun config conn ->
  let*? _ = R.Methods.resume_with_token ~token:config.rocketchat_token conn in
  let*? resp = R.Methods.get_rooms conn in
  let print_room : R.Room.t -> unit Lwt.t = function
   | Direct_chat_room room -> Lwt_io.printlf "D %s" room.id
   | Chat_room room -> Lwt_io.printlf "C %s %S" room.id room.name
   | Private_room room -> Lwt_io.printlf "P %s %S" room.id room.name
   | Livechat_room room -> Lwt_io.printlf "L %s" room.id
  in
  Lwt_list.iter_s print_room resp.update >|= Result.ok
end

let get_rooms_cmd =
  let open Cmdliner in
  let term = Term.(const get_rooms $ config_arg) in
  Cmd.v (Cmd.info "get-rooms") term

(* Main *)

let () =
  let open Cmdliner.Cmd in
  exit @@ eval' @@ group (info "rocketcmd") [
    login_cmd;
    get_rooms_cmd;
  ]
