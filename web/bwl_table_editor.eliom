(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared
  open Eliom_client
  open Eliom_content.Html
  open Lwt.Infix
  open Unprime_option
  open Printf

  module type ELEMENT_SHARED = sig

    type t [@@deriving json]

    include Prime_enumset.OrderedType with type t := t

  end

  module Make_shared (E : ELEMENT_SHARED) = struct

    type update = Add of E.t | Remove of E.t | Replace of E.t * E.t

    type serverside =
        (unit, (E.t list, string) result) server_function
      * (E.t option * E.t, (unit, string) result) server_function
      * (E.t, (unit, string) result) server_function
      * update Eliom_comet.Channel.t

    type clientside = [`Div] elt -> serverside -> unit

  end
]

[%%client
  open Js_of_ocaml
  open Js_of_ocaml_lwt

  module type ELEMENT = sig
    include ELEMENT_SHARED

    type edit_dom

    val render_headers : unit -> [> `Th] elt list
    val render_row : t -> [`Td] elt list
    val render_edit_row : t option -> edit_dom * [`Td] elt list
    val decode_row : t option -> edit_dom -> t
  end

  module Make (E : ELEMENT) = struct
    include Make_shared (E)
    module Enset = Prime_enumset.Make (E)

    let content, set_content = React.S.create Enset.empty

    let clientside : clientside =
      fun outer_div (sf_fetch_all, sf_add, sf_remove, update_stream) ->

      let enset = ref Enset.empty in
      let editing = ref None (* or Some (pos, row, elt_opt) *) in

      let new_button = D.(button ~a:[D.a_button_type `Button] [txt "new"]) in
      let top_outside_td = D.(td ~a:[a_class ["outside"]] [new_button]) in
      let top_tr = D.tr (E.render_headers () @ [top_outside_td]) in
      let table = D.(table ~a:[a_class ["edit"]] [top_tr]) in
      let table_dom = To_dom.of_table table in

      let row_pos i =
        match !editing with
        | Some (pos, _, None, _) when 1 + i >= pos -> 2 + i
        | _ -> 1 + i in

      let rec render_row pos row elt =
        row##.innerHTML := Js.string "";
        List.iter
          (fun cell ->
            let cell_dom = To_dom.of_td cell in
            ignore (Dom.appendChild row cell_dom))
          (E.render_row elt);
        let on_edit ev = enable_edit pos row (Some elt) in
        let on_remove ev =
          Lwt.async begin fun () ->
            sf_remove elt >|= function
            | Ok () -> ()
            | Error msg -> Dom_html.window##alert(Js.string msg)
          end in
        let outside_td =
          D.td ~a:[D.a_class ["outside"]] [
            D.button ~a:[D.a_button_type `Button; D.a_onclick on_edit]
                     [D.txt "edit"];
            D.button ~a:[D.a_button_type `Button; D.a_onclick on_remove]
                     [D.txt "remove"]
          ] in
        Dom.appendChild row (To_dom.of_td outside_td)

      and render_edit_row_outside ?(is_removed = false) row elt_opt edit_dom =
        let on_cancel ev = disable_edit () in
        let on_add ev =
          Lwt.async begin fun () ->
            sf_add (elt_opt, E.decode_row elt_opt edit_dom) >|= function
            | Ok () -> on_cancel ev
            | Error msg -> Dom_html.window##alert(Js.string msg)
          end in
        let commit_label =
          if is_removed then "re-add" else
          if elt_opt = None then "add" else
          "update" in
        let outside_td = D.td ~a:[D.a_class ["outside"]] [
          D.button ~a:[D.a_button_type `Button; D.a_onclick on_cancel]
                   [D.txt "cancel"];
          D.button ~a:[D.a_button_type `Button; D.a_onclick on_add]
                   [D.txt commit_label];
        ] in
        Dom.appendChild row (To_dom.of_td outside_td)

      and render_edit_row row elt_opt =
        row##.innerHTML := Js.string "";
        let edit_dom, edit_tds = E.render_edit_row elt_opt in
        List.iter (fun cell -> Dom.appendChild row (To_dom.of_td cell))
                  edit_tds;
        render_edit_row_outside row elt_opt edit_dom;
        edit_dom

      and disable_edit () =
        begin match !editing with
        | None -> ()
        | Some (_, row, None, _) -> Dom.removeChild table_dom row;
        | Some (pos, row, Some elt, _) -> render_row pos row elt
        end;
        editing := None

      and enable_edit pos row elt_opt =
        if !editing <> None then disable_edit ();
        let edit_dom = render_edit_row row elt_opt in
        editing := Some (pos, row, elt_opt, edit_dom) in

      let on_new _ _ =
        enable_edit 1 (table_dom##insertRow 1) None; Lwt.return_unit in
      Lwt_js_events.(async
        (fun () -> clicks (To_dom.of_element new_button) on_new));

      let add elt =
        let i, row =
          match Enset.locate elt !enset with
          | false, _ ->
            enset := Enset.add elt !enset;
            set_content !enset;
            let i = snd (Enset.locate elt !enset) in
            i, table_dom##insertRow (row_pos i)
          | true, i ->
            let row = Js.Opt.get (table_dom##.rows##item (row_pos i))
                                 (fun () -> failwith "Js.Opt.get") in
            i, row in
        render_row (row_pos i) row elt in

      let remove elt =
        match Enset.locate elt !enset with
        | false, _ -> Eliom_lib.error "No element matches delete request."
        | true, i ->
          enset := Enset.remove elt !enset;
          set_content !enset;
          begin match !editing with
          | Some (pos, row, Some _, edit_dom) when pos = 1 + i ->
            editing := Some (pos, row, None, edit_dom);
            Js.Opt.iter (row##.lastChild) (Dom.removeChild row);
            render_edit_row_outside ~is_removed:true row None edit_dom
          | _ ->
            table_dom##deleteRow (row_pos i)
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
          | Error msg -> Dom_html.window##alert(Js.string msg));

      Manip.appendChild outer_div table
  end

]

[%%server
  let section = Lwt_log.Section.make "Bwl_table_editor"

  module type ELEMENT = sig
    include ELEMENT_SHARED

    val which_type : string
    val fetch_all : unit -> t list Lwt.t
    val add : t option -> t -> t Lwt.t
    val remove : t -> unit Lwt.t
  end

  module Make (E : ELEMENT) = struct
    include Make_shared (E)

    let update_stream, emit = Lwt_stream.create ()
    let update_comet : update Eliom_comet.Channel.t =
      Eliom_comet.Channel.create ~scope:Eliom_common.site_scope update_stream

    let fetch_all = server_function [%json: unit]
      begin fun () ->
        try%lwt E.fetch_all () >|= fun entries -> Ok entries
        with
         | Caqti_error.Exn err ->
            Lwt.return (Error (Caqti_error.show err))
         | exn ->
            let msg = sprintf "Failed to fetch %s list." E.which_type in
            Lwt_log.error ~exn ~section msg >>= fun () -> Lwt.return (Error msg)
      end

    let add = server_function [%json: E.t option * E.t]
      begin fun (old_entry_opt, entry) ->
        try%lwt
          E.add old_entry_opt entry >|= fun entry ->
          Option.iter (fun entry -> emit (Some (Remove entry))) old_entry_opt;
          emit (Some (Add entry));
          Ok ()
        with
         | Caqti_error.Exn err ->
            Lwt.return (Error (Caqti_error.show err))
         | exn ->
            let msg = sprintf "Failed to add %s." E.which_type in
            Lwt_log.error ~exn ~section msg >>= fun () -> Lwt.return (Error msg)
      end

    let remove = server_function [%json: E.t]
      begin fun entry ->
        try%lwt
          E.remove entry >|= fun () -> emit (Some (Remove entry)); Ok ()
        with
         | Caqti_error.Exn err ->
            Lwt.return (Error (Caqti_error.show err))
         | exn ->
            let msg = sprintf "Failed to remove %s." E.which_type in
            Lwt_log.error ~exn ~section msg >>= fun () -> Lwt.return (Error msg)
      end

    let serverside = fetch_all, add, remove, update_comet
  end
]
