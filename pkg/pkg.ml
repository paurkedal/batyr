#! /usr/bin/env
#use "topfind"
#require "topkg-jbuilder"

open Topkg

let licenses = List.map Pkg.std_file ["COPYING"]

let () = Topkg_jbuilder.describe ~licenses ~name:"batyr-core" ()
