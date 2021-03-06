.PHONY: all clean lint run-web

srcdir = $(shell pwd)

all:
	dune build
	export OCAMLPATH=$(srcdir)/_build/install/default/lib \
	  && ocaml pkg/pkg_web.ml build --build-dir _build/ocamlbuild

clean:
	dune clean

lint:
	topkg lint; topkg lint --pkg-file=pkg/pkg_web.ml

run-web: all
	install -d -m755 _var/{lib,log,run} _config
	export OCAMLPATH=$(srcdir)/_build/install/default/lib \
	  && ocsigenserver -c web/ocsigen-dev.conf
