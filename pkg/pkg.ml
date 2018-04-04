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
     | "batyr" -> ["-I"; "lib"; "-I"; "on-xmpp"; "-tag"; "internal_deps"]
     | "batyr-lib" -> []
     | "batyr-web" | "batyr-on-slack" | "batyr-on-xmpp" ->
        ["-tag"; "external_deps"]
     | _ -> assert false) in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(eliom.ocamlbuild)"
        % "-build-dir" % build_dir
        %% of_list self_dep
        %% of_list targets)

let build = Pkg.build ~cmd:build_cmd ()

let metas = [
  Pkg.meta_file ~install:false "pkg/META.batyr-lib";
  Pkg.meta_file ~install:false "pkg/META.batyr-on-xmpp";
  Pkg.meta_file ~install:false "pkg/META.web";
]

let batyr_lib_only_deps = [
  "extunix";
]

let batyr_web_only_deps = [
  "eliom";
  "js_of_ocaml";
  "js_of_ocaml-ppx";
]

let batyr_on_slack_only_deps = [
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

let batyr_lib_nondeps =
  ["yojson"] @ batyr_web_only_deps @ batyr_on_slack_only_deps
let batyr_web_nondeps =
  ["batyr"] @ batyr_lib_only_deps @ batyr_on_slack_only_deps
let batyr_on_xmpp_nondeps =
  ["batyr"] @ batyr_lib_only_deps @ batyr_web_only_deps
let batyr_on_slack_nondeps =
  ["batyr"; "erm_xmpp"] @ batyr_lib_only_deps @ batyr_web_only_deps

let opams =
  let install = false in
  Pkg.[
    opam_file ~install ~lint_deps_excluding:(Some batyr_lib_nondeps)
      "batyr-lib.opam";
    opam_file ~install ~lint_deps_excluding:(Some batyr_on_xmpp_nondeps)
      "batyr-on-xmpp.opam";
    opam_file ~install ~lint_deps_excluding:(Some batyr_on_slack_nondeps)
      "batyr-on-slack.opam";
    opam_file ~install ~lint_deps_excluding:(Some batyr_web_nondeps)
      "batyr-web.opam";
  ]

let save_list ?(pfx = "") fn lines =
  let oc = open_out fn in
  List.iter (fun m -> output_string oc (pfx ^ m ^ "\n")) lines;
  close_out oc

let internal_tag = Tags.singleton "internal"

let () = Pkg.describe ~build ~metas ~opams "batyr" @@ fun c ->
  Modules.of_file "lib/batyr-lib.oclib"
    >>= fun batyr_modules ->
  Modules.save batyr_modules "doc/api.odocl"
    >>= fun () ->
  Modules.(of_file ~tags:internal_tag "web/server/batyrweb.oclib")
    >>= fun batyr_web_modules ->
  Modules.mllib batyr_web_modules "web/server/batyrweb.mllib"
    >>= fun batyr_web_mllib ->
  Modules.save Modules.(union batyr_modules batyr_web_modules) "doc/dev.odocl"
    >>= fun () ->
  Modules.mllib batyr_modules "lib/batyr-lib.mllib"
    >>= fun batyr_mllib ->
  Modules.of_file "on-xmpp/batyr-on-xmpp.oclib"
    >>= fun batyr_on_xmpp_modules ->
  Modules.mllib batyr_on_xmpp_modules "on-xmpp/batyr-on-xmpp.mllib"
    >>= fun batyr_on_xmpp_mllib ->
  let batyr_lib_targets = [
    Pkg.lib "batyr-lib.opam" ~dst:"opam";
    Pkg.lib "pkg/META.batyr-lib" ~dst:"META";
    batyr_mllib;
  ] in
  let batyr_web_targets = [
    Pkg.lib "batyr-web.opam" ~dst:"opam";
    Pkg.lib "pkg/META.batyr-web" ~dst:"META";
    batyr_web_mllib;
    Pkg.share ~dst:"static/" "web/client/batyrweb_main.js";
    Pkg.share ~dst:"static/" "web/client/batyrweb_admin.js";
    Pkg.share ~dst:"static/css/" "web/static/css/batyr.css";
  ] in
  let batyr_on_xmpp_targets = [
    Pkg.lib "batyr-on-xmpp.opam" ~dst:"opam";
    Pkg.lib "pkg/META.batyr-on-xmpp" ~dst:"META";
    batyr_on_xmpp_mllib;
  ] in
  let batyr_on_slack_targets = [
    Pkg.lib "batyr-on-slack.opam" ~dst:"opam";
    Pkg.bin ~dst:"batyr-on-slack" "on-slack/main";
  ] in
  (match Conf.pkg_name c with
   | "batyr" ->
      Ok (batyr_lib_targets @ batyr_on_xmpp_targets @ batyr_on_slack_targets @
          batyr_web_targets)
   | "batyr-lib" -> Ok batyr_lib_targets
   | "batyr-on-xmpp" -> Ok batyr_on_xmpp_targets
   | "batyr-on-slack" -> Ok batyr_on_slack_targets
   | "batyr-web" -> Ok batyr_web_targets
   | other -> R.error_msgf "unknown package name: %s" other)
