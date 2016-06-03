#! /usr/bin/env ocaml

(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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
#require "topkg"
#require "unix"

open Topkg

let build_cmd c os =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-classic-display"
        % "-plugin-tag" % "package(ocamlbuild-eliom-dev)"
        % "-build-dir" % build_dir
        % "lib/META")

let build = Pkg.build ~cmd:build_cmd ()

let metas = [Pkg.meta_file "pkg/META"]

let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["lib"]) "opam"]

let batyr_api = [
  "Batyr_cache";
  "Batyr_data";
  "Batyr_db";
  "Batyr_prereq";
  "Batyr_presence";
  "Batyr_search";
  "Batyr_version";
  "Batyr_xmpp";
]

let () = Unix.putenv "OCAMLPATH" Filename.(concat (Unix.getcwd ()) "_build")

let () = Pkg.describe ~build ~metas ~opams "batyr" @@ fun c ->
  Ok [
    Pkg.mllib ~api:batyr_api "lib/batyr.mllib";
    Pkg.mllib ~api:[] ~dst_dir:"web/" "web/server/batyrweb.mllib";
    Pkg.share ~dst:"static/" "web/client/batyrweb_main.js";
    Pkg.share ~dst:"static/" "web/client/batyrweb_admin.js";
    Pkg.share ~dst:"static/css/" "web/static/css/batyr.css";
  ]
