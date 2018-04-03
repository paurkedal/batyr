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
  let self_dep =
    (match Conf.pkg_name c with
     | "batyr" -> ["-I"; "lib"; "-tag"; "internal_deps"]
     | "batyr-lib" -> []
     | "batyr-web" | "batyr-logger-slack" -> ["-tag"; "external_deps"]
     | _ -> assert false) in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(eliom.ocamlbuild)"
        % "-build-dir" % build_dir
        %% of_list self_dep
        %% of_list targets)
let () = Unix.putenv "OCAMLPATH" "lib"

let build = Pkg.build ~cmd:build_cmd ()

let metas = [Pkg.meta_file "pkg/META"]

let lib_only_deps = [
  "extunix";
]

let web_only_deps = [
  "eliom";
  "js_of_ocaml";
  "js_of_ocaml-ppx";
]

let slack_only_deps = [
  "cmdliner";
  "conduit";
  "conduit-lwt";
  "dns-lwt-unix";
  "logs";
  "lru";
  "slacko";
  "kojson";
  "ptime";
  "uri";
  "websocket";
  "websocket-lwt";
]

let lib_nondeps = ["yojson"] @ web_only_deps @ slack_only_deps
let web_nondeps = ["batyr"] @ lib_only_deps @ slack_only_deps
let slack_nondeps = ["batyr"; "erm_xmpp"] @ lib_only_deps @ web_only_deps

let opams =
  let install = false in
  Pkg.[
    opam_file ~install ~lint_deps_excluding:(Some lib_nondeps) "batyr-lib.opam";
    opam_file ~install ~lint_deps_excluding:(Some web_nondeps) "batyr-web.opam";
    opam_file ~install ~lint_deps_excluding:(Some slack_nondeps)
      "batyr-logger-slack.opam";
  ]

let save_list ?(pfx = "") fn lines =
  let oc = open_out fn in
  List.iter (fun m -> output_string oc (pfx ^ m ^ "\n")) lines;
  close_out oc

let internal_tag = Tags.singleton "internal"

let () = Pkg.describe ~build ~metas ~opams "batyr" @@ fun c ->
  Modules.of_file "lib/batyr.oclib"
    >>= fun batyr_modules ->
  Modules.save batyr_modules "doc/api.odocl"
    >>= fun () ->
  Modules.(of_file ~tags:internal_tag "web/server/batyrweb.oclib")
    >>= fun batyrweb_modules ->
  Modules.mllib batyrweb_modules "web/server/batyrweb.mllib"
    >>= fun batyrweb_mllib ->
  Modules.save Modules.(union batyr_modules batyrweb_modules) "doc/dev.odocl"
    >>= fun () ->
  Modules.mllib batyr_modules "lib/batyr.mllib"
    >>= fun batyr_mllib ->
  let batyr_targets = [
    Pkg.lib "pkg/META";
    Pkg.lib "batyr-lib.opam" ~dst:"opam";
    batyr_mllib;
  ] in
  let batyrweb_targets = [
    batyrweb_mllib;
    Pkg.lib "batyr-web.opam" ~dst:"opam";
    Pkg.share ~dst:"static/" "web/client/batyrweb_main.js";
    Pkg.share ~dst:"static/" "web/client/batyrweb_admin.js";
    Pkg.share ~dst:"static/css/" "web/static/css/batyr.css";
  ] in
  let batyr_logger_slack_targets = [
    Pkg.lib "batyr-logger-slack.opam" ~dst:"opam";
    Pkg.bin ~dst:"batyr-logger-slack" "logger-slack/main";
  ] in
  (match Conf.pkg_name c with
   | "batyr" ->
      Ok (batyr_targets @ batyrweb_targets @ batyr_logger_slack_targets)
   | "batyr-lib" ->
      Ok batyr_targets
   | "batyr-web" ->
      Ok batyrweb_targets
   | "batyr-logger-slack" ->
      Ok batyr_logger_slack_targets
   | other ->
      R.error_msgf "unknown package name: %s" other)
