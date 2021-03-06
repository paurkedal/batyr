(* Copyright (C) 2013--2019  Petter A. Urkedal <paurkedal@gmail.com>
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
open Js_of_ocaml
open Lwt.Infix
open Html.D
open Printf
open Unprime_option

let pager ?default_index ?count labels draw_inner =
  let shown_index = ref (-1) in
  let sheet_div = div ~a:[a_class ["sheet"]] [] in
  let sheet_dom = Html.To_dom.of_div sheet_div in
  let tabs_dom = ref [||] in
  let update_content i =
    if i <> !shown_index then begin
      if !shown_index >= 0 then
        !tabs_dom.(!shown_index)##.classList##remove(Js.string "selected");
      !tabs_dom.(i)##.classList##add(Js.string "selected");
      shown_index := i;
      sheet_dom##.innerHTML := Js.string "";
      Lwt.async (fun () ->
        draw_inner i >|=
        List.iter (fun el -> Dom.appendChild sheet_dom
                                             (Html.To_dom.of_element el)))
    end in
  let c_max =
    match count with
    | None -> 0
    | Some count ->
      Prime_int.fold_to (fun i -> max (count i)) (Array.length labels) 1 in
  let make_page i l =
    let onclick _ = update_content i in
    let cls = if i = !shown_index then ["selected"] else [] in
    match count with
    | None ->
      span ~a:[a_onclick onclick; a_class ("b-tab" :: cls)] [txt l]
    | Some count ->
      let c = count i in
      let cc = sprintf "b-x%x" (15 * c / c_max) in
      if c = 0 then
        span ~a:[a_onclick onclick;
                 a_class ("b-tab" :: "empty" :: cc :: "b-link" :: cls)]
          [txt l; span ~a:[a_class ["b-bar"]] []]
      else
        span ~a:[a_class ("b-tab" :: cc :: cls)] [
          sup ~a:[a_class ["b-tab-count"]] [txt (string_of_int c)];
          span ~a:[a_onclick onclick;
                   a_class ["b-link"]]
            [txt l];
        ] in
  let tabs = List.mapi make_page (Array.to_list labels) in
  tabs_dom := Array.map Html.To_dom.of_span (Array.of_list tabs);
  Option.iter update_content default_index;
  Lwt.return
    [div ~a:[a_class ["b-pager"]]
      [div ~a:[a_class ["b-tabbar"]] tabs;
       div ~a:[a_class ["b-tabbar-clear"]] [];
       sheet_div]
    ]
