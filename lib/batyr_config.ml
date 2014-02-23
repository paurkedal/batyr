(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

open Config_file

let group = new group

let db_host_cp = new option_cp string_wrappers ~group ["db"; "host"]
  None "Host name or socket directory of the PostgreSQL server."
let db_hostaddr_cp = new option_cp string_wrappers ~group ["db"; "hostaddr"]
  None "An IP number to use instead of resolving the host parameter."
let db_port_cp = new option_cp string_wrappers ~group ["db"; "port"]
  None "Port to which to connect or socket file name extension."
let db_database_cp = new option_cp string_wrappers ~group ["db"; "database"]
  None "The name of the database."
let db_user_cp = new option_cp string_wrappers ~group ["db"; "user"]
  None "The database user to authenticate as."
let db_password_cp = new option_cp string_wrappers ~group ["db"; "password"]
  None "The password by which to authenticate."

let () =
  let section = Lwt_log.Section.make "Batyr.config" in
  let fp =
    try Unix.getenv "BATYR_CONF" with Not_found -> "/etc/batyr/batyr.conf" in
  try group#read fp
  with Sys_error reason ->
    Lwt_log.ign_warning ~section reason
