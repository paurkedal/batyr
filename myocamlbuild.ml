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

let js_of_ocaml_version =
  let ic = Unix.open_process_in "js_of_ocaml --version" in
  let version = input_line ic in
  (match Unix.close_process_in ic with
   | Unix.WEXITED 0 -> ()
   | _ -> failwith "js_of_ocaml --version failed");
  (match String.split_on_char '.' (String.trim version) with
   | [] | [_] -> failwith "Failed to parse js_of_ocaml version."
   | v0 :: v1 :: _ -> (int_of_string v0, int_of_string v1))

let () = dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  (match hook with
   | Before_options -> Options.make_links := false
   | After_rules ->
      if js_of_ocaml_version >= (3, 6) then
        flag ["js_of_ocaml"] & S[A"+js_of_ocaml-compiler/runtime.js"]
   | _ -> ())
