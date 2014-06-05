(* OASIS_START *)
(* OASIS_STOP *)

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "web/client/main.byte";
  "web/client/admin.byte";
]

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  dispatch_default hook;
  M.dispatcher ~oasis_executables hook;
  match hook with
  | Before_options ->
    Options.make_links := false
  | After_rules ->
    (* Cf https://github.com/ocsigen/js_of_ocaml/issues/20 *)
    flag ["js_of_ocaml"] &
      S[A"+eliom/client/eliom_client.js"; A"+js_of_ocaml/weak.js"]
  | _ -> ()
