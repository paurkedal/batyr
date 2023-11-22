(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_request.Infix
open Caqti_type.Std
module type CONNECTION = Caqti_lwt.CONNECTION

include (val Batyr_core.Data.connect Config.global.storage_uri)

let rooms =
  let req =
    unit -->* t5 int string string (option string) bool @:-
    "SELECT DISTINCT node_id, domain_name, node_name, room_alias, \
                     transcribe \
     FROM batyr.muc_rooms NATURAL JOIN batyr.nodes \
                          NATURAL JOIN batyr.domains \
     ORDER BY domain_name DESC, node_name DESC, room_alias DESC"
  in
  fun () ->
    Db.use_exn @@ function (module C : CONNECTION) ->
    C.fold req List.cons () []
