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
      % "-X" % "_build" (* jbuilder *)
      %% of_list targets
  )

let build = Pkg.build ~cmd:build_cmd ()
let metas = [Pkg.meta_file ~install:false "pkg/META.batyr-web"]
let opams = [Pkg.opam_file ~install:false "batyr-web.opam"]
let licenses = [Pkg.std_file "COPYING"]

let () = Pkg.describe ~build ~metas ~opams ~licenses "batyr-web" @@ fun c ->
  Ok [
    Pkg.lib "batyr-web.opam" ~dst:"opam";
    Pkg.lib "pkg/META.batyr-web" ~dst:"META";
    Pkg.mllib ~api:[] "web/server/batyrweb.mllib";
    Pkg.share ~dst:"static/" "web/client/batyrweb_main.js";
    Pkg.share ~dst:"static/" "web/client/batyrweb_admin.js";
    Pkg.share ~dst:"static/css/" "web/static/css/batyr.css";
  ]
