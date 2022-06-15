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

open Lwt.Infix

open Content

let help_span =
  let bop s = H.span ~a:[H.a_class ["bnf-op"]] [H.txt s] in
  let op s = H.span [bop "'"; H.txt s; bop "'"] in
  let vn s = H.span ~a:[H.a_style "font-style: italic"] [H.txt s] in
  let sp = H.txt " " in
  H.span ~a:[H.a_class ["help"]] [
    vn"pat"; sp; bop"::="; sp;
    bop"("; op"author:"; sp; bop"|"; sp; op"subject:"; sp; bop"|";
            op"body:"; bop")"; bop"?"; sp;
    bop"("; vn"word"; sp; bop"|"; sp;
            op"\""; vn"string"; op"\""; sp; bop")"; sp; bop"|"; sp;
    op"("; vn"pat"; op")"; sp; bop"|"; sp;
    op"!"; vn"pat"; sp; bop"|"; sp;
    vn"pat"; sp; vn"pat"; sp; bop"|"; sp;
    vn"pat"; sp; op"|"; sp; vn"pat"
  ]

let render room_jid room =
  let min_time =
    Prime_option.get_else
      Unix.time (Data.Muc_room.min_message_time room)
  in
  let info_span = H.span [] ~a:[H.a_class ["error"]] in
  let clear_button =
    H.a [H.txt "all"] ~a:[H.a_href (Vpaths.room room_jid)]
  in
  let search_button =
    H.button [H.txt "matching"] ~a:[H.a_button_type `Submit]
  in
  let search_input =
    H.input () ~a:[H.a_name "pattern"; H.a_input_type `Text]
  in
  page ~title:(Fmt.str "Transcript for %s" room_jid) [
    H.form ~a:[H.a_method `Get] [
      H.txt "Show "; clear_button;
      H.txt " or "; search_button; search_input; H.txt " ";
      info_span; H.txt " "; help_span;
    ];
    H.div [] ~a:[
      H.a_id "batyr.room.messages";
      H.a_user_data "batyr-room" room_jid;
      H.a_user_data "batyr-min-seen-time" (string_of_float min_time);
    ];
    H.script ~a:[H.a_src (Vpaths.batyr_js)] (H.txt "");
  ]

let handle request =
  let room_jid = Dream.param request "room_jid" in
  let room_node = Data.Node.of_string room_jid in
  Data.Muc_room.stored_of_node room_node >>= function
   | None -> Dream.not_found request
   | Some room -> render room_jid room
