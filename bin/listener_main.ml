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

type launch_result =
  [ `Signalled of int
  | `Failed_to_connect
  | `Lost_connection ]

module type LISTENER = sig
  type config

  val config_of_jsonm : Ezjsonm.value -> config

  val launch : config -> [> launch_result] Lwt.t
end

module Make (Listener : LISTENER) = struct

  let main config_path =
    let backoff = Batyr_backoff.create () in
    Lwt_main.run begin
      let rec start () =
        let%lwt config_string =
          Lwt_io.with_file ~mode:Lwt_io.input config_path Lwt_io.read in
        let%lwt config_json = Lwt.wrap1 Ezjsonm.from_string config_string in
        let config = Listener.config_of_jsonm (Ezjsonm.value config_json) in
        let rec keep_alive () =
          (match%lwt Listener.launch config with
           | `Signalled 1 -> (* HUP *)
              Logs_lwt.info (fun p ->
                p "Reloading config and reconnecting due to SIGHUP.")
                >>= fun () ->
              start ()
           | `Signalled sn ->
              Logs_lwt.info (fun p -> p "Session terminated due to signal %d." sn)
           | `Failed_to_connect | `Lost_connection ->
              let dt = Batyr_backoff.next backoff in
              Logs_lwt.info (fun p -> p "Reconnecting in %.3g s." dt) >>= fun () ->
              Lwt_unix.sleep dt >>= fun () ->
              keep_alive ()) in
        keep_alive () in
      start ()
    end

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
     | `Ok () -> exit 0
     | `Version | `Help -> exit 0)
end
