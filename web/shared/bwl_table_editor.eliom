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
  open Unprime_option
  open Batyrweb_prereq
  open Printf

  module type ELEMENT_SHARED = sig

    type t deriving (Json)

    include Prime_enumset.OrderedType with type t := t

  end

  module Make_shared (E : ELEMENT_SHARED) = struct

    type update = Add of E.t | Remove of E.t | Replace of E.t * E.t

    type clientside =
      [`Div] Html5.elt ->
	(unit -> E.t list fallible Lwt.t)
      * (E.t -> unit fallible Lwt.t)
      * (E.t -> unit fallible Lwt.t)
      * update Lwt_stream.t ->
      unit

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

  module Make (E : ELEMENT) = struct
    include Make_shared (E)
    module Enset = Prime_enumset.Make (E)

    let clientside : clientside =
      fun outer_div (sf_fetch_all, sf_add, sf_remove, update_stream) ->

      let enset = ref Enset.empty in
      let editing = ref None (* or Some (pos, is_new) *) in

      let new_button = Html5.D.(button ~button_type:`Button [pcdata "new"]) in
      let top_outside_td = Html5.D.td [new_button] in
      let top_tr = Html5.D.tr (E.render_headers () @ [top_outside_td]) in
      let table = Html5.D.(table ~a:[a_class ["edit"]] top_tr []) in
      let table_dom = Html5.To_dom.of_table table in

      let on_new _ _ =
	begin match !editing with
	| Some _ -> ()
	| None ->
	  let edit_dom, edit_tds = E.render_edit_row None in
	  let row = table_dom##insertRow(1) in
	  let on_cancel ev =
	    table_dom##deleteRow(1);
	    editing := None in
	  let on_add ev =
	    Lwt.async begin fun () ->
	      sf_add (E.decode_row None edit_dom) >|= function
	      | Ok () -> on_cancel ev
	      | Failed msg -> assert false (* FIXME *)
	    end in
	  let outside_td = Html5.D.(td [
	    button ~a:[a_onclick on_add] ~button_type:`Button [pcdata "add"];
	    button ~a:[a_onclick on_cancel] ~button_type:`Button [pcdata "cancel"];
	  ]) in
	  List.iter
	    (fun cell -> Dom.appendChild row (Html5.To_dom.of_td cell))
	    (edit_tds @ [outside_td]);
	  editing := Some (1, true)
	end;
	Lwt.return_unit in
      Lwt_js_events.(async
	(fun () -> clicks (Html5.To_dom.of_element new_button) on_new));

      let row_pos i =
	match !editing with
	| Some (j, true) when i >= j -> 2 + i
	| _ -> 1 + i in

      let add elt =
	let row =
	  match Enset.locate elt !enset with
	  | None ->
	    enset := Enset.add elt !enset;
	    let i = Option.get (Enset.locate elt !enset) in
	    table_dom##insertRow (row_pos i)
	  | Some i ->
	    let row = Js.Opt.get (table_dom##rows##item(row_pos i))
				 (fun () -> failwith "Js.Opt.get") in
	    row##innerHTML <- Js.string ""; row in
	List.iter
	  (fun cell ->
	    let cell_dom = Html5.To_dom.of_td cell in
	    ignore (Dom.appendChild row cell_dom))
	  (E.render_row elt);
	let on_remove ev =
	  Lwt.async begin fun () ->
	    sf_remove elt >|= function
	    | Ok () -> ()
	    | Failed msg -> assert false (* FIXME *)
	  end in
	let outside_td = Html5.D.(
	  td ~a:[a_class ["outside"]] [
	    button ~a:[a_onclick on_remove]
		   ~button_type:`Button [pcdata "remove"]
	  ]) in
	Dom.appendChild row (Html5.To_dom.of_td outside_td) in

      let remove elt =
	match Enset.locate elt !enset with
	| None -> Eliom_lib.error "No element matches delete request."
	| Some i ->
	  enset := Enset.remove elt !enset;
	  table_dom##deleteRow (row_pos i);
	  begin match !editing with
	  | Some (j, false) when j = i -> editing := Some (j, true)
	  | _ -> ()
	  end in

      Lwt.async begin fun () ->
	Lwt_stream.iter
	  begin function
	  | Add elt -> add elt
	  | Remove elt -> remove elt
	  | Replace (elt, elt') -> remove elt; add elt'
	  end
	  update_stream
      end;

      Lwt.ignore_result
	(sf_fetch_all () >|= function
	  | Ok entries -> List.iter add entries
	  | Failed msg -> assert false (* FIXME *));

      Html5.Manip.appendChild outer_div table
  end

}}

{server{
  module type ELEMENT = sig
    include ELEMENT_SHARED

    val which_type : string
    val fetch_all : unit -> t list Lwt.t
    val add : t -> unit Lwt.t
    val remove : t -> unit Lwt.t
  end

  module Make (E : ELEMENT) = struct
    include Make_shared (E)

    let update_stream, emit = Lwt_stream.create ()
    let update_comet : update Eliom_comet.Channel.t =
      Eliom_comet.Channel.create ~scope:Eliom_common.site_scope update_stream

    let fetch_all = server_function Json.t<unit>
      begin fun () ->
        try_lwt E.fetch_all () >|= fun entries -> Ok entries
        with xc ->
          let msg = sprintf "Failed to fetch %s list." E.which_type in
          Lwt_log.error msg >> Lwt.return (Failed msg)
      end

    let add = server_function Json.t<E.t>
      begin fun entry ->
        try_lwt
          E.add entry >|= fun () -> emit (Some (Add entry)); Ok ()
        with xc ->
          let msg = sprintf "Failed to add %s." E.which_type in
          Lwt_log.error msg >> Lwt.return (Failed msg)
      end

    let remove = server_function Json.t<E.t>
      begin fun entry ->
        try_lwt
          E.remove entry >|= fun () -> emit (Some (Remove entry)); Ok ()
        with xc ->
          let msg = sprintf "Failed to remove %s." E.which_type in
          Lwt_log.error msg >> Lwt.return (Failed msg)
      end

    let serverside = fetch_all, add, remove, update_comet

    let create clientside serverside =
      let outer_div = Html5.D.div [] in
      {unit{React.E.fix (%clientside %outer_div %serverside)}};
      outer_div
  end
}}
