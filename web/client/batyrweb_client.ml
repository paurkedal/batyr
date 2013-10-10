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
open Unprime_option

let (>|=) = Lwt.(>|=)

module type LIVE_TABLE_ELEMENT = sig
  include Prime_enumset.OrderedType
  val render_row : t -> [`Td] Html5.elt list
end

module Live_table (Elt : LIVE_TABLE_ELEMENT) = struct
  module Enset = Prime_enumset.Make (Elt)

  type t = {
    mutable enset : Enset.t;
    table : Dom_html.tableElement Js.t;
    static_row_count : int;
  }

  let create tbl = {
    enset = Enset.empty;
    table = tbl;
    static_row_count = tbl##rows##length;
  }

  let add t elt =
    let row =
      match Enset.locate elt t.enset with
      | None ->
	t.enset <- Enset.add elt t.enset;
	let i = Option.get (Enset.locate elt t.enset) in
	t.table##insertRow (t.static_row_count + i)
      | Some i ->
	let row =
	  Js.Opt.get (t.table##rows##item (t.static_row_count + 1))
		     (fun () -> failwith "Js.Opt.get") in
	row##innerHTML <- Js.string ""; row in
    List.iter
      (fun cell ->
	ignore (row##appendChild((Html5.To_dom.of_td cell :> Dom.node Js.t))))
      (Elt.render_row elt)

  let remove t elt =
    match Enset.locate elt t.enset with
    | None -> Eliom_lib.error "Element to delete not found."
    | Some i ->
      t.enset <- Enset.remove elt t.enset;
      t.table##deleteRow (t.static_row_count + i)
end
