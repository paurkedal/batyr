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

open Eliom_content

(* A complete Html5.R implementation will be available in an upcoming version
 * of Eliom, at which point this will be removed. *)
module Html5_R : sig

  val retain : 'a -> 'b -> unit

  val signal_is_const : 'a React.signal -> bool

  val append_child_signals :
    #Dom.node Js.t -> 'a Eliom_content.Html5.elt React.signal list -> unit

  val div :
    ?a: [< Html5_types.div_attrib] Html5.attrib list ->
    [< Html5_types.div_content_fun] Html5.elt React.signal list ->
    [> `Div] Html5.elt

  val span :
    ?a: [< Html5_types.span_attrib] Html5.attrib list ->
    [< Html5_types.span_content_fun] Html5.elt React.signal list ->
    [> `Span] Html5.elt

  val simple_select :
    ?a: [< Html5_types.select_attrib] Html5.attrib list ->
    [< Html5_types.select_content_fun] Html5.elt React.signal list ->
    [> `Select] Html5.elt

end
