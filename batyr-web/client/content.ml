(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open Lwt.Infix
open Printf
open Unprime_option
module H = Html

let pager ?default_index ?count labels draw_inner =
  let shown_index = ref (-1) in
  let sheet_div = H.div ~a:[H.a_class ["sheet"]] [] in
  let sheet_dom = To_dom.of_div sheet_div in
  let tabs_dom = ref [||] in
  let update_content i =
    if i <> !shown_index then
      begin
        if !shown_index >= 0 then
          !tabs_dom.(!shown_index)##.classList##remove(Js.string "selected");
        !tabs_dom.(i)##.classList##add(Js.string "selected");
        shown_index := i;
        sheet_dom##.innerHTML := Js.string "";
        Lwt.async begin fun () ->
          draw_inner i >|=
          List.iter (fun el -> Dom.appendChild sheet_dom (To_dom.of_element el))
        end
      end
  in
  let c_max =
    (match count with
     | None -> 0
     | Some count ->
        Prime_int.fold_to (fun i -> max (count i)) (Array.length labels) 1)
  in
  let make_page i l =
    let onclick _ = update_content i; true in
    let cls = if i = !shown_index then ["selected"] else [] in
    (match count with
     | None ->
        H.span ~a:[H.a_onclick onclick; H.a_class ("b-tab" :: cls)] [H.txt l]
     | Some count ->
        let c = count i in
        let cc = sprintf "b-x%x" (15 * c / c_max) in
        if c = 0 then
          H.span [H.txt l; H.span ~a:[H.a_class ["b-bar"]] []]
            ~a:[
              H.a_onclick onclick;
              H.a_class ("b-tab" :: "empty" :: cc :: "b-link" :: cls)
            ]
        else
          H.span ~a:[H.a_class ("b-tab" :: cc :: cls)] [
            H.sup ~a:[H.a_class ["b-tab-count"]] [H.txt (string_of_int c)];
            H.span ~a:[H.a_onclick onclick; H.a_class ["b-link"]] [H.txt l];
          ])
  in
  let tabs = List.mapi make_page (Array.to_list labels) in
  tabs_dom := Array.map To_dom.of_span (Array.of_list tabs);
  Option.iter update_content default_index;
  Lwt.return [
    H.div ~a:[H.a_class ["b-pager"]] [
      H.div ~a:[H.a_class ["b-tabbar"]] tabs;
      H.div ~a:[H.a_class ["b-tabbar-clear"]] [];
      sheet_div
    ]
  ]
