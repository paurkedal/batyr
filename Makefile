.PHONY: all clean run-web

srcdir = $(shell pwd)

all:
	jbuilder build --dev
	export OCAMLPATH=$(srcdir)/_build/install/default/lib \
	  && cd web \
	  && ocaml pkg/pkg.ml build --build-dir $(srcdir)/_build/web

clean:
	jbuilder clean

run-web: all
	install -d -m755 _var/{lib,log,run} _config
	export OCAMLPATH=$(srcdir)/_build/install/default/lib \
	  && ocsigenserver -c web/ocsigen-dev.conf
