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

let () =
  rule "pkg/META -> lib/META"
    ~dep:"pkg/META" ~prod:"lib/META"
    begin fun env build ->
      Cmd (S[A"sed"; A"/^\\s*requires =/ s/\\<batyr\\>/lib/g";
             P"pkg/META"; Sh">"; Px"lib/META"])
    end;
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
  copy_rule ".mllib -> .odocl" "lib/%.mllib" "lib/%.odocl"

let () = dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  match hook with
  | Before_options -> Options.make_links := false
  | After_rules ->
    dep ["ocaml"; "ocamldep"; "package(lib)"] ["lib/batyr.otarget"]
  | _ -> ()
