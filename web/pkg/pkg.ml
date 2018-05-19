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
#require "topkg"
#require "unix"

open Topkg

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@ Cmd.(
    ocamlbuild
      % "-use-ocamlfind"
      % "-plugin-tag" % "package(eliom.ocamlbuild)"
      % "-build-dir" % build_dir
      %% of_list targets
  )

let build = Pkg.build ~cmd:build_cmd ()
let metas = [Pkg.meta_file ~install:false "pkg/META.batyr-web"]
let opams = [Pkg.opam_file ~install:false "batyr-web.opam"]

let () =
  (* FIXME: The Change log, license files, and README are located in the parent
   * directory. We can't refer to them here. *)
  Pkg.describe ~build ~metas ~opams ~change_logs:[] ~licenses:[] ~readmes:[]
               "batyr-web" @@ fun c ->
  Ok [
    Pkg.lib "batyr-web.opam" ~dst:"opam";
    Pkg.lib "pkg/META.batyr-web" ~dst:"META";
    Pkg.mllib ~api:[] "server/batyrweb.mllib";
    Pkg.share ~dst:"static/" "client/batyrweb_main.js";
    Pkg.share ~dst:"static/" "client/batyrweb_admin.js";
    Pkg.share ~dst:"static/css/" "static/css/batyr.css";
  ]
