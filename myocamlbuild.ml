open Ocamlbuild_plugin

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "web/client/batyrweb_main.byte";
  "web/client/batyrweb_admin.byte";
]

let sed_rule ~dep ~prod scripts =
  rule (dep ^ " -> " ^ prod) ~dep ~prod
    (fun env build ->
      let dep = env dep and prod = env prod in
      let script_args = List.map (fun script -> S[A"-e"; A script]) scripts in
      Cmd (S[A"sed"; S script_args; P dep; Sh">"; Px prod]))

let meta_sed = ["/^\\s*requires =/ s/\\<batyr\\>/lib/g"]

let () =
  sed_rule ~dep:"pkg/META.batyr-%" ~prod:"%/META.batyr-%" meta_sed;
  rule "%.mli & %.idem -> %.ml"
    ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
    begin fun env build ->
      let src = env "%.mli" and dst = env "%.ml" in
      cp src dst
    end;
  rule "%.eliomi & %.idem -> %.eliom"
    ~deps:["%.eliomi"; "%.idem"] ~prod:"%.eliom"
    begin fun env build ->
      let src = env "%.eliomi" and dst = env "%.eliom" in
      cp src dst
    end;
  sed_rule ~dep:"%.olib" ~prod:"%.mllib" ["s/^[+-]//"];
  sed_rule ~dep:"%.olib" ~prod:"%.odocl" ["s/^+//"; "s/^-.*//"]

let () = dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  (match hook with
   | Before_options -> Options.make_links := false
   | After_rules ->
      dep ["internal_deps"; "ocaml"; "ocamldep"; "use_batyr_lib"]
          ["lib/batyr-lib.otarget"];
      dep ["internal_deps"; "ocaml"; "ocamldep"; "use_batyr_on_xmpp"]
          ["on-xmpp/batyr-on-xmpp.otarget"];
      List.iter
        (fun phase ->
          flag ["external_deps"; "ocaml"; phase; "use_batyr_lib"]
               (S[A"-package"; A"batyr-lib"]);
          flag ["external_deps"; "ocaml"; phase; "use_batyr_on_xmpp"]
               (S[A"-package"; A"batyr-on-xmpp"]))
        ["ocamldep"; "compile"; "infer_interface"; "link"]
   | _ -> ())
