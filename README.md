# Batyr - A Chat Logger and Viewer


## Synopsis

Batyr is a bot which logs messages from chatrooms to a PostgreSQL database
and provides a web interface for browsing and searching the them.

Please note that development of this project is sporadic and that there is
insufficient documentation.


## Installation

Prepare an OPAM installation as described in http://opam.ocaml.org/, then

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install batyr-core batyr-web batyr-on-rocketchat batyr-on-slack batyr-on-xmpp

where irrelevant `batyr-on-*` packages can be omitted.

To build from the git repository, add `--deps-only` to the last command and
issue `dune build` from the source directory.


## Deployment

This guide is incomplete.

Create a PostgreSQL database, say `batyr`, which can be accessed by the user
running the Ocsigen server, and prepare it with

    psql -U batyr batyr -f $prefix/share/batyr/schemas/batyr_base.sql
    psql -U batyr batyr -f $prefix/share/batyr/schemas/batyr_proc.sql

The first file contains the table definitions.  The second contains views
and functions which can be updated without touching any data.

The run the web interface can be started with

    export BATYR_CONFIG=<config-file>
    batyr-web

where the configuration is a JSON encoding of
[batyr-web/server/config.mli](batyr-web/server/config.mli).

For recording messages, the following commands are available:

    batyr-on-rocketchat <config-file>
    batyr-on-slack <config-file>
    batyr-on-xmpp <config-file>

Each take a configuration file of different undocumented format as argument.
See the respective `config.mli` files.
