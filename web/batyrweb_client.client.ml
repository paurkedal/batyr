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

open Js_of_ocaml
open Lwt.Infix
open Eliom_content
open Unprime_option

module Domx_html = struct
  let element_by_id : (#Dom_html.element Js.t -> 'a Js.opt) -> string -> 'a
                    = fun coercion id ->
    Js.coerce_opt
      (Dom_html.document##getElementById(Js.string id))
      coercion (fun _ -> assert false)
end

module Caltime = struct
  let to_epoch jd = 0.001 *. Js.to_float jd##valueOf

  let day_start d =
    new%js Js.date_day d##getFullYear d##getMonth d##getDate
  let day_end d =
    new%js Js.date_day d##getFullYear d##getMonth (d##getDate + 1)
  let month_start d = new%js Js.date_month d##getFullYear d##getMonth
  let month_end d = new%js Js.date_month d##getFullYear (d##getMonth + 1)
  let year_start d = new%js Js.date_month d##getFullYear 0
  let year_end d = new%js Js.date_month (d##getFullYear + 1) 0

  let days_in_month d =
    (new%js Js.date_day d##getFullYear (d##getMonth + 1) (-1))##getDate + 1

  let month_names = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                      "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]
  let day_names = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]
end

module type LIVE_TABLE_ELEMENT = sig
  include Prime_enumset.OrderedType
  val render_row : t -> [`Td] Html.elt list
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
    static_row_count = tbl##.rows##.length;
  }

  let add t elt =
    let row =
      match Enset.locate elt t.enset with
      | false, _ ->
        t.enset <- Enset.add elt t.enset;
        let i = snd (Enset.locate elt t.enset) in
        t.table##insertRow (t.static_row_count + i)
      | true, i ->
        let row =
          Js.Opt.get (t.table##.rows##item (t.static_row_count + i))
                     (fun () -> failwith "Js.Opt.get") in
        row##.innerHTML := Js.string ""; row in
    List.iter
      (fun cell ->
        ignore (row##appendChild((Html.To_dom.of_td cell :> Dom.node Js.t))))
      (Elt.render_row elt)

  let remove t elt =
    match Enset.locate elt t.enset with
    | false, _ -> Eliom_lib.error "Element to delete not found."
    | true, i ->
      t.enset <- Enset.remove elt t.enset;
      t.table##deleteRow (t.static_row_count + i)
end
