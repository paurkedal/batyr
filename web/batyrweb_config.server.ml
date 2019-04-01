(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Global configuration. *)

open Config_file

let group = new group

let db_uri_cp = new string_cp ~group ["db_uri"] "postgresql://"
  "Caqti URI for connecting to database."

let hide_passwords_cp = new bool_cp ~group ["admin"; "hide_passwords"] true
  "Hide the account passwords in the admin interface."

let () =
  let section = Lwt_log.Section.make "Batyr.config" in
  let fp =
    try Unix.getenv "BATYR_CONF" with Not_found -> "/etc/batyr/batyr.conf" in
  try group#read fp
  with Sys_error reason ->
    Lwt_log.ign_warning ~section reason
