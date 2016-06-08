.PHONY: doc test all install uninstall clean distclean

prefix = $(shell opam config var prefix)

OCAMLBUILD_PLUGINS = -plugin-tag 'package(ocamlbuild-eliom-dev)'
OCAMLBUILD = OCAMLPATH=. ocamlbuild -use-ocamlfind $(OCAMLBUILD_PLUGINS)

all:
	ocaml pkg/pkg.ml build

clean:
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) lib/batyr.docdir/index.html

install:
	opam-installer --prefix $(prefix) batyr.install

uninstall:
	opam-installer --prefix $(prefix) -u batyr.install

distclean: clean
	rm -f batyr.install
