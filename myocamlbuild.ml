(*# require ocamlbuild-eliom *)
(*# require oasis.base *)

open Ocamlbuild_plugin
open Ocamlbuild_eliom
open Ocamlbuild_ocsigen

let server_packages =
  ["config-file"; "erm_xmpp"; "postgresql"; "prime"; "prime.testing"]
let local_server_packages = ["lib/batyr"]
let local_eliom_packages = ["web/server/batyrweb"]

let () = dispatch begin function
  | Before_options ->
    Options.use_ocamlfind := true;
    Options.make_links := false

  | After_rules ->
    enable_eliom_rules ();
    enable_ocsigen_conf_rules ~server_subdir:"web/server"
	~local_server_packages ~server_packages ~local_eliom_packages ();
    Pathname.define_context "web/server" ["web"];
    Pathname.define_context "web/client" ["web"];
    flag ["ocaml"; "link"; "library"; "thread"] & A"-thread";
    eliom_lib ~dir:"lib" "batyr";
    Pathname.define_context "web/server" ["lib"]

  | _ -> ()
end
