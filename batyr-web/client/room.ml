(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Syntax
open Printf
open Unprime
open Unprime_list
open Unprime_option
open Unprime_string

open Common

module H = Html

type chatroom = string
let query_limit = 10000

module Chatroom = struct
  type t = chatroom
  let compare = String.compare
  let render_row room = [H.td [H.txt room]]
end
module Chatrooms_live = Live_table (Chatroom)

let fetch_message_counts =
  make_call Protocol.count_messages_path
    Protocol.count_messages_request_to_yojson
    Protocol.count_messages_response_of_yojson

let fetch_transcript =
  make_call Protocol.fetch_messages_path
    Protocol.fetch_messages_request_to_yojson
    Protocol.fetch_messages_response_of_yojson

module Message_counts = struct

  type t = {
    mutable card: int;
    mutable branches: t array;
  }

  let card mc = mc.card

  let year_span mc = Array.length mc.branches

  let get mc i = mc.branches.(i)

  let fetch room pattern y_min y_now tz =
    let mc =
      let init_mday _ = {card = 0; branches = [||]} in
      let init_month _ = {card = 0; branches = Array.init 31 init_mday} in
      let init_year _ = {card = 0; branches = Array.init 12 init_month} in
      {card = 0; branches = Array.init (y_now - y_min + 1) init_year}
    in
    fetch_message_counts {room; pattern; tz} >|= function
     | Ok counts ->
        List.iter
          (fun {Protocol.date; count} ->
            let y = date lsr 16 in
            let m = date lsr 8 land 0xff in
            let d = date land 0xff in
            let mcY = mc.branches.(y - y_min) in
            let mcM = mcY.branches.(m - 1) in
            let mcD = mcM.branches.(d - 1) in
            mcD.card <- count;
            mcM.card <- mcM.card + count;
            mcY.card <- mcY.card + count)
          counts;
        Ok mc
     | Error msg -> Error msg
end

module Transcript = struct

  type t = {
    mutable ts_day: int * int * int;
    mutable ts_day_str: string;
    ts_dom: Dom_html.divElement Js.t;
    mutable ts_updater: unit Lwt.t option;
  }

  let create () =
    let transcript_div = H.div [] in
    let ts = {
      ts_day = (0, 0, 0);
      ts_day_str = "";
      ts_dom = To_dom.of_div transcript_div;
      ts_updater = None;
    } in
    (transcript_div, ts)

  let append_message ts msg =
    let msg_frag =
      (match msg.Protocol.msg_body with
       | None -> []
       | Some body -> [H.span ~a:[H.a_class ["body"]] [H.txt body]])
    in
    let msg_frag =
      (match
        (match msg.msg_subject, msg.msg_thread with
         | None, None -> None
         | None, Some thread ->
            Some [H.txt (sprintf "[- %s] " thread)]
         | Some subject, None ->
            Some [H.txt (sprintf "[%s] " subject)]
         | Some subject, Some thread ->
            Some [H.txt (sprintf "[%s - %s] " subject thread)])
       with
       | None -> msg_frag
       | Some st_frag ->
          H.span ~a:[H.a_class ["subject"]] st_frag :: msg_frag)
    in
    let msg_frag =
      (match msg.msg_edit_time with
       | None -> msg_frag
       | Some _ ->
          msg_frag @ [
            H.txt " ";
            H.span ~a:[H.a_class ["edited"]] [H.txt "(edited)"]
          ])
    in
    let jstime =
      new%js Js.date_fromTimeValue ((Ptime.to_float_s msg.msg_time) *. 1000.0)
    in
    let day = jstime##getFullYear, jstime##getMonth + 1, jstime##getDate in
    if day <> ts.ts_day then begin
      let (y, m, d), wd = day, Caltime.day_names.(jstime##getDay) in
      let day_str = sprintf "%04d-%02d-%02d" y m d in
      let header_h2 = H.h2 [H.txt (wd ^ " " ^ day_str)] in
      Dom.appendChild ts.ts_dom (To_dom.of_h2 header_h2);
      ts.ts_day <- day;
      ts.ts_day_str <- day_str
    end;
    let h, m, s = jstime##getHours, jstime##getMinutes, jstime##getSeconds in
    let hour = sprintf "%02d:%02d:%02d" h m s in
    let frag = sprintf "%sT%02d%02d%02d" ts.ts_day_str h m s in
    let message_p =
      H.p ~a:[H.a_class ["message"]; H.a_id frag] (
        H.span ~a:[H.a_class ["hour"]] [H.txt hour] :: H.txt " " ::
        H.span ~a:[H.a_class ["sender"; msg.msg_sender_cls]]
          [H.txt msg.msg_sender] :: H.txt ": " ::
        msg_frag
      )
    in
    Dom.appendChild ts.ts_dom (To_dom.of_p message_p)

  let append_messages ts msgs =
    let message_count = List.length msgs in
    List.iter (append_message ts) msgs;
    if message_count = query_limit then begin
      let limit_s =
        sprintf "This result has been limited to %d messages." query_limit
      in
      let limit_p = H.(p ~a:[a_class ["warning"]] [txt limit_s]) in
      Dom.appendChild ts.ts_dom (To_dom.of_p limit_p)
    end

  let enable_updates ts update_comet =
    if ts.ts_updater = None then begin
      Log_async.debug (fun f -> f "Enabling updates.");
      ts.ts_updater <- Some (Lwt_stream.iter (append_message ts) update_comet)
    end

  let disable_updates ts =
    (match ts.ts_updater with
     | None -> ()
     | Some updater ->
        Log_async.debug (fun f -> f "Disabling updates.");
        ts.ts_updater <- None;
        Lwt.cancel updater)
end

let shown_transcript = ref None

let render_transcript ~room ?tI ?tF ?pattern ?date update_comet =
  (match !shown_transcript with
   | None -> ()
   | Some ts -> Transcript.disable_updates ts; shown_transcript := None);
  fetch_transcript {room; time_start = tI; time_stop = tF; pattern} >|= function
   | Error msg ->
      [H.p ~a:[H.a_class ["error"]] [H.txt msg]]
   | Ok msgs ->
      let content, ts = Transcript.create () in
      Transcript.append_messages ts msgs;

      let is_past date =
        let jt_now = new%js Js.date_now in
        Caltime.day_start date <> Caltime.day_start jt_now
      in
      if not (Option.for_all is_past date) then
        Transcript.enable_updates ts update_comet;

      shown_transcript := Some ts;
      [content]

type shown_counts = {
  sc_room: string;
  sc_pattern: string option;
  sc_counts: Message_counts.t;
}
let shown_counts = ref None

let render_page ~room ~min_time ?pattern ~page transcript_dom update_comet =
  let jt_min = new%js Js.date_fromTimeValue (min_time *. 1000.0) in
  let jt_now = new%js Js.date_now in
  let y_min = jt_min##getFullYear in
  let y_now = jt_now##getFullYear in

  let render_day iY iM iD =
    let date = new%js Js.date_day (y_min + iY) iM (1 + iD) in
    let tI = Caltime.to_epoch (Caltime.day_start date) in
    let tF = Caltime.to_epoch (Caltime.day_end date) in
    page := [iY; iM; iD];
    render_transcript ~room ~tI ~tF ?pattern ~date update_comet
  in

  let render_month mcY iY iM = function
   | [] ->
      let date = new%js Js.date_month (y_min + iY) iM in
      let tI = Caltime.to_epoch (Caltime.month_start date) in
      let tF = Caltime.to_epoch (Caltime.month_end date) in
      page := [iY; iM];
      render_transcript ~room ~tI ~tF ?pattern ~date update_comet
   | [iD] ->
      let date = new%js Js.date_month (y_min + iY) iM in
      let mcM = Message_counts.get mcY iM in
      let label iD = sprintf "%d" (iD + 1) in
      Content.pager
        ~default_index:iD
        ~count:(fun iD -> Message_counts.(card (get mcM iD)))
        (Array.init (Caltime.days_in_month date) label)
        (fun iD -> render_day iY iM iD)
   | _ -> assert false
  in

  let render_year mcA iY = function
   | [] ->
      let tI = Caltime.to_epoch (new%js Js.date_month (y_min + iY) 0) in
      let tF = Caltime.to_epoch (new%js Js.date_month (y_min + iY + 1) 0) in
      page := [iY];
      render_transcript ~room ~tI ~tF ?pattern update_comet
   | iM :: iDs ->
      let mcY = Message_counts.get mcA iY in
      Content.pager
        ~default_index:iM
        ~count:(fun iM -> Message_counts.(card (get mcY iM)))
        Caltime.month_names
        (fun iM -> render_month mcY iY iM iDs)
  in

  let render_all mcA = function
   | [] ->
      render_transcript ~room ?pattern update_comet
   | iY :: iMDs ->
      let label iY = sprintf "%04d" (y_min + iY) in
      Content.pager
        ~default_index:iY
        ~count:(fun iY -> Message_counts.(card (get mcA iY)))
        (Array.init (y_now - y_min + 1) label)
        (fun iY -> render_year mcA iY iMDs)
  in

  let is_current =
    (match !shown_counts with
     | Some sc ->
        sc.sc_room = room && sc.sc_pattern = pattern
         && Message_counts.year_span sc.sc_counts >= (y_now - y_min + 1)
     | None -> false)
  in

  if is_current then Lwt.return_ok () else begin
    (* FIXME: Need the real timezone for this to work across DST. *)
    let today = new%js Js.date_now in
    let tz = sprintf "%d" (today##getTimezoneOffset / 60) in
    Message_counts.fetch room pattern y_min y_now tz >>= function
     | Ok counts ->
        let+ content = render_all counts !page in
        shown_counts := Some {
          sc_room = room;
          sc_pattern = pattern;
          sc_counts = counts;
        };
        transcript_dom##.innerHTML := Js.string "";
        List.iter (Dom.appendChild transcript_dom % To_dom.of_element) content;
        Ok ()
     | Error msg ->
        Lwt.return_error msg
  end

let start () =
  (match Dom_html.getElementById_opt "batyr.room.messages" with
   | None ->
      Log_async.info (fun f -> f "batyr.room.messages not found")
   | Some elem ->
      Log_async.info (fun f -> f "batyr.room.messages found");
      let room =
        elem##getAttribute (Js.string "data-batyr-room")
          |> Fun.flip Js.Opt.get (fun () -> assert false)
          |> Js.to_string
      in
      let min_time =
        elem##getAttribute (Js.string "data-batyr-min-seen-time")
          |> Fun.flip Js.Opt.get (fun () -> assert false)
          |> Js.to_string
          |> float_of_string
      in
      let pattern = Uri.get_query_param (current_url ()) "pattern" in
      let page = ref [0; 0; 0] (* FIXME: Last. *) in
      let blocked_stream =
        let p, _ = Lwt.wait () in
        Lwt_stream.return_lwt p
      in
      Lwt.ignore_result
        (render_page ~room ~min_time ?pattern ~page elem blocked_stream))
