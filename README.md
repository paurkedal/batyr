# Batyr - A Chat Logger and Viewer


## Synopsis

Batyr is a bot which logs messages from chatrooms to a PostgreSQL database
and provides a web interface for browsing and searching the them.  The web
interface runs under [Ocsigen][ocsigen].


## Installation

Prepare an OPAM installation as described in http://opam.ocaml.org/, then

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install batyr

If you wish to try it in a local Git clone, add `--deps-only` to the last
command, then in the top source directory:

    ocaml pkg/pkg.ml build


## Deployment

See the [Ocsigen manual][ocsigen] for how to setup up the web server.  A
sample configuration for Batyr is installed under `$prefix/share/batyr`.
Create a PostgreSQL database, say `batyr`, which can be accessed by the user
running the Ocsigen server, and prepare it with

    psql -U batyr batyr -f $prefix/share/batyr/schemas/batyr_base.sql
    psql -U batyr batyr -f $prefix/share/batyr/schemas/batyr_proc.sql

The first file contains the table definitions.  The second contains views
and functions which can be updated without touching any data.  Then point to
this database in the configuration file `/etc/batyr/batyr.conf`:
```
(* Caqti URI for connecting to database. *)
db_uri = "postgresql://batyr@db-host/batyr"

admin = {
  (* Hide the account passwords in the admin interface. *)
  hide_passwords = true
}
```
A database URI `postgresql:/` would give you the ident-authenticated default
database.

[ocsigen]: http://ocsigen.org/ocsigenserver/
[pgenv]: http://www.postgresql.org/docs/9.3/interactive/libpq-envars.html
