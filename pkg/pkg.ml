#! /usr/bin/env ocaml

(* Copyright (C) 2016--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

#use "topfind"
#require "adpkg"
#require "topkg"
#require "unix"

open Adpkg
open Topkg

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(eliom.ocamlbuild)"
        % "-build-dir" % build_dir
        % "lib/META"
        %% of_list targets)
let () = Unix.putenv "OCAMLPATH" "."

let build = Pkg.build ~cmd:build_cmd ()

let metas = [Pkg.meta_file "pkg/META"]

let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["lib"]) "batyr.opam"]

let save_list ?(pfx = "") fn lines =
  let oc = open_out fn in
  List.iter (fun m -> output_string oc (pfx ^ m ^ "\n")) lines;
  close_out oc

let () = Pkg.describe ~build ~metas ~opams "batyr" @@ fun c ->
  Modules.of_file "lib/batyr.oclib"
    >>= fun batyr_modules ->
  Modules.(of_file ~tags:(Tags.singleton "internal") "web/server/batyrweb.oclib")
    >>= fun batyrweb_modules ->
  Modules.save batyr_modules "doc/api.odocl"
    >>= fun () ->
  Modules.save Modules.(union batyr_modules batyrweb_modules) "doc/dev.odocl"
    >>= fun () ->
  Modules.mllib batyr_modules "lib/batyr.mllib"
    >>= fun batyr_mllib ->
  Modules.mllib batyrweb_modules ~dst_dir:"web/" "web/server/batyrweb.mllib"
    >>= fun batyrweb_mllib ->
  Ok [
    batyr_mllib;
    Pkg.bin ~dst:"batyr-logger-slack" "logger-slack/main";
    batyrweb_mllib;
    Pkg.share ~dst:"static/" "web/client/batyrweb_main.js";
    Pkg.share ~dst:"static/" "web/client/batyrweb_admin.js";
    Pkg.share ~dst:"static/css/" "web/static/css/batyr.css";
  ]
