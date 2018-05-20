open Ocamlbuild_plugin

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "client/batyrweb_main.byte";
  "client/batyrweb_admin.byte";
]

let () =
  copy_rule ".server.ml -> .ml" "%.server.ml" "server/%.ml";
  copy_rule ".client.ml -> .ml" "%.client.ml" "client/%.ml";
  copy_rule "%.shared.ml -> client/%.ml" "%.shared.ml" "client/%.ml";
  copy_rule "%.shared.ml -> server/%.ml" "%.shared.ml" "server/%.ml";
  copy_rule ".server.mli -> .mli" "%.server.mli" "server/%.mli";
  copy_rule ".client.mli -> .mli" "%.client.mli" "client/%.mli";
  copy_rule "%.shared.mli -> client/%.mli" "%.shared.mli" "client/%.mli";
  copy_rule "%.shared.mli -> server/%.mli" "%.shared.mli" "server/%.mli"

let () = dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  (match hook with
   | Before_options -> Options.make_links := false
   | _ -> ())
