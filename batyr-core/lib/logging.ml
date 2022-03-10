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

module Verbosity = struct
  type t = {
    global: Logs.level option option;
    per_source: (string * Logs.level option) list;
  }

  let default = {global = None; per_source = []}

  let merge v1 v2 =
    let global =
      (match v1.global, v2.global with
       | None, None -> None
       | Some global, _ | None, Some global -> Some global)
    in
    let per_source = v1.per_source @ v2.per_source in
    {global; per_source}

  let level_of_string_exn level_name =
    (match Logs.level_of_string level_name with
     | Ok level -> level
     | Error (`Msg msg) -> failwith msg)

  let of_string s =
    try
      let parse_pair x =
        let i = String.rindex x ':' in
        let level_name = String.sub x (i + 1) (String.length x - i - 1) in
        (String.sub x 0 i, level_of_string_exn level_name)
      in
      (match String.split_on_char ',' s with
       | [] -> Ok {global = None; per_source = []}
       | "" :: xs -> Ok {global = None; per_source = List.map parse_pair xs}
       | x :: xs when not (String.contains x ':') ->
          Ok {global = Some (level_of_string_exn x);
              per_source = List.map parse_pair xs}
       | xs -> Ok {global = None; per_source = List.map parse_pair xs})
    with
     | Failure msg -> Error (`Msg ("invalid verbosity: " ^ msg))
     | Not_found -> Error (`Msg "missing colon in verbosity specification")

  let pp =
    let open Fmt in
    let level = option ~none:(const string "quiet") Logs.pp_level in
    using (fun {global; _} -> global) (option level) ++
    using (fun {per_source; _} -> per_source)
      (list (comma ++ pair ~sep:(const string ":") string level))

  let cmdliner_conv = Cmdliner.Arg.conv (of_string, pp)

  let cmdliner_term =
    let open Cmdliner in
    let doc = "Set log default and per-source verbosity." in
    let docv = "[LEVEL][,SRC:LEVEL,...]" in
    let env = Cmd.Env.info ~doc "BATYR_LOG_VERBOSITY" in
    Arg.(value @@ opt cmdliner_conv default @@
         info ~env ~doc ~docv ["verbosity"])
end

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

let setup ?(verbosity = Verbosity.default) () =
  Logs.set_reporter (lwt_reporter ());
  let setup_src src =
    Option.iter
      (Logs.Src.set_level src)
      (List.assoc_opt (Logs.Src.name src) verbosity.per_source)
  in
  Option.iter Logs.set_level verbosity.global;
  List.iter setup_src (Logs.Src.list ())
