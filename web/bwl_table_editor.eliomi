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

{shared{
  open Eliom_content

  module type ELEMENT_SHARED = sig
    type t deriving (Json)
    include Prime_enumset.OrderedType with type t := t
  end
}}

{client{
  module type ELEMENT = sig
    include ELEMENT_SHARED

    type edit_dom

    val render_headers : unit -> [> `Th] Html5.elt list
    val render_row : t -> [`Td] Html5.elt list
    val render_edit_row : t option -> edit_dom * [`Td] Html5.elt list
    val decode_row : t option -> edit_dom -> t
  end

  module Make (E : ELEMENT) : sig
    type serverside
    type clientside = [`Div] Html5.elt -> serverside -> unit
    val clientside : clientside

    module Enset : Prime_enumset.S with type elt = E.t

    val content : Enset.t React.signal
  end
}}

{server{
  module type ELEMENT = sig
    include ELEMENT_SHARED

    val which_type : string
    val fetch_all : unit -> t list Lwt.t
    val add : t option -> t -> t Lwt.t
    val remove : t -> unit Lwt.t
  end

  module Make (E : ELEMENT) : sig
    type clientside
    type serverside
    val serverside : serverside
  end
}}
