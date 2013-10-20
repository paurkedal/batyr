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

{shared{
  open Batyrweb_prereq
  open Eliom_content
}}

{server{
  module D' = struct
    open Html5.D

    let page title content =
      Eliom_tools.D.html ~title ~css:[["css"; "batyr.css"]]
	(body (h1 [pcdata title] :: content))

    let tabbar
	  ~(onclick : (int -> string -> Dom_html.mouseEvent Js.t -> unit)
		      Eliom_lib.client_value)
	  labels =
      let make_tab i l = span ~a:[a_onclick {{ %onclick %i %l }}] [pcdata l] in
      (div ~a:[a_class ["tabbar"]] [
	div ~a:[a_class ["tabs"]] (List.mapi make_tab labels);
	div ~a:[a_class ["header"]] []
      ])
  end
}}

{client{
  open Html5.D

  module D' = struct
    let pager ?(default_index = 0) labels draw_inner =
      let shown_index = ref (-1) in
      let sheet_div = div ~a:[a_class ["sheet"]] [] in
      let sheet_dom = Html5.To_dom.of_div sheet_div in
      let tabs_dom = ref [||] in
      let onclick i l ev =
	if i <> !shown_index then begin
	  if !shown_index >= 0 then
	    !tabs_dom.(!shown_index)##className <- Js.string "";
	  !tabs_dom.(i)##className <- Js.string "selected";
	  shown_index := i;
	  sheet_dom##innerHTML <- Js.string "";
	  Lwt.async (fun () ->
	    draw_inner i l >|=
	    List.iter (fun el -> Dom.appendChild sheet_dom
						 (Html5.To_dom.of_element el)))
	end in
      let make_page i l =
	let cls = if i = !shown_index then ["selected"] else [] in
	span ~a:[a_onclick (onclick i l); a_class cls] [pcdata l] in
      let tabs = List.mapi make_page (Array.to_list labels) in
      tabs_dom := Array.map Html5.To_dom.of_span (Array.of_list tabs);
      (*onclick default_index labels.(default_index) >|= fun () ->*)
      Lwt.return
	[div ~a:[a_class ["pager"]]
	  [div ~a:[a_class ["tabs"]] tabs;
	   div ~a:[a_class ["clear"]] [];
	   sheet_div]
	]
  end
}}

{shared{
  module D = struct
    include D'
  end
}}
