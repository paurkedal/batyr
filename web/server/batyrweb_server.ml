(* Copyright (C) 2013--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content
open Eliom_pervasives

let ignore_client_unit : unit client_value -> unit = fun _ -> ()

module Main_app =
  Eliom_registration.App (struct let application_name = "batyrweb_main" end)

let index_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let transcript_service =
  Eliom_service.App.service ~path:["rooms"]
    ~get_params:Eliom_parameter.
      (suffix_prod (string "chatroom") (opt (float "tI") ** opt (float "tF") **
                                        opt (string "pat")))
    ()

let admin_service =
  Eliom_service.App.service ~path:["admin"] ~get_params:Eliom_parameter.unit ()

let status_service =
  Eliom_service.Http.service ~path:["status"]
    ~get_params:Eliom_parameter.unit ()

let () = Lwt.async Batyr_presence.Session.start_all
