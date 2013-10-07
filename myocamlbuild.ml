(*# require ocamlbuild-eliom *)
(*# require oasis.base *)

open Ocamlbuild_plugin
open Ocamlbuild_eliom
open Ocamlbuild_ocsigen

let server_packages = ["erm_xmpp"; "prime"; "prime.testing"]

let () = dispatch begin function
  | Before_options ->
    Options.use_ocamlfind := true;
    Options.make_links := false

  | After_rules ->
    enable_eliom_rules ();
    enable_ocsigen_conf_rules ~server_subdir:"web/server" ~server_packages ();
    Pathname.define_context "web/server" ["web"];
    Pathname.define_context "web/client" ["web"];
    flag ["ocaml"; "link"; "library"; "thread"] & A"-thread"

  | _ -> ()
end
