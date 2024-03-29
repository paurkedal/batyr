(* Copyright (C) 2018--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module Log = (val Logs_lwt.src_log (Logs.Src.create "batyr-core"))

type launch_result =
  [ `Exit of int
  | `Signalled of int
  | `Failed_to_connect
  | `Lost_connection ]

module type LISTENER = sig
  module Config : sig
    type t
    val load : string -> (t, [`Msg of string]) result Lwt.t
    val verbosity : t -> Logging.Verbosity.t
  end

  val launch : Config.t -> [> launch_result] Lwt.t
end

module Make (Listener : LISTENER) = struct

  let main config =
    let backoff = Backoff.create () in
    Lwt_main.run begin
      let rec restart () =
        Listener.launch config >>= function
         | `Failed_to_connect | `Lost_connection ->
            let dt = Backoff.next backoff in
            Log.err (fun f ->
              f "No connection, reconnecting in %.3g s." dt) >>= fun () ->
            Lwt_unix.sleep dt >>= fun () ->
            restart ()
         | `Signalled 1 -> (* HUP *)
            Log.info (fun f ->
              f "Reloading config and reconnecting due to SIGHUP.")
              >>= fun () ->
            restart ()
         | `Signalled sn ->
            Log.info (fun f -> f "Session terminated due to signal %d." sn)
         | `Exit n ->
            Log.info (fun f -> f "Exiting with exit code %d." n) >|= fun () ->
            exit n
      in
      restart ()
    end

  let setup_logging config_path verbosity_arg =
    let config = Lwt_main.run begin
      Listener.Config.load config_path >>= function
       | Ok config -> Lwt.return config
       | Error (`Msg msg) ->
          ksprintf Lwt.fail_with
            "Cannot load configuration %s: %s" config_path msg
    end in
    let verbosity_cfg = Listener.Config.verbosity config in
    let verbosity = Logging.Verbosity.merge verbosity_arg verbosity_cfg in
    Logging.setup ~verbosity ();
    config

  let main_cmd =
    let open Cmdliner in
    let config =
      Arg.(required (pos 0 (some string) None (info ~docv:"CONFIG" [])))
    in
    let term =
      let open Term in
      const main
        $ (const setup_logging $ config $ Logging.Verbosity.cmdliner_term)
    in
    let info = Cmd.info (Filename.basename Sys.argv.(0)) in
    Cmd.v info term

  let () = exit (Cmdliner.Cmd.eval main_cmd)
end
