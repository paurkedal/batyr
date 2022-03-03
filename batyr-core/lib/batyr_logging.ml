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

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    let flush () =
      let m = Buffer.contents b in
      Buffer.reset b; m
    in
    (Fmt.with_buffer ~like b, flush)
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
       | Logs.App ->
          Lwt_io.write Lwt_io.stdout (app_flush ()) >>= fun () ->
          Lwt_io.flush Lwt_io.stdout
       | _ ->
          Lwt_io.write Lwt_io.stderr (dst_flush ()) >>= fun () ->
          Lwt_io.flush Lwt_io.stderr
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  {Logs.report = report}
