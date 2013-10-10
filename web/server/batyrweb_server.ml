(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let admin_service =
  Eliom_service.service ~path:["admin"] ~get_params:Eliom_parameter.unit ()

module Layout = struct
  module D = struct
    let page title content =
      Eliom_tools.D.html ~title ~css:[["css"; "batyr.css"]]
	(Html5.D.body (Html5.D.h1 [Html5.D.pcdata title] :: content))
  end
end
