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

open Eliom_content

(* A complete Html.R implementation will be available in an upcoming version
 * of Eliom, at which point this will be removed. *)
module Html_R : sig

  val retain : 'a -> 'b -> unit

  val signal_is_const : 'a React.signal -> bool

  val append_child_signals :
    #Dom.node Js.t -> 'a Eliom_content.Html.elt React.signal list -> unit

  val div :
    ?a: [< Html_types.div_attrib] Html.attrib list ->
    [< Html_types.div_content_fun] Html.elt React.signal list ->
    [> `Div] Html.elt

  val span :
    ?a: [< Html_types.span_attrib] Html.attrib list ->
    [< Html_types.span_content_fun] Html.elt React.signal list ->
    [> `Span] Html.elt

end
