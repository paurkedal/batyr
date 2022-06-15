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

open Lwt.Syntax

open Content

let index _ =
  let* rooms = Data.rooms () in
  let render_room (_node_id, domain_name, node_name, alias, _transcribe) =
    let node_jid = node_name ^ "@" ^ domain_name in
    let label =
      (match alias with
       | None -> [H.txt node_jid]
       | Some alias -> [H.txt alias; H.txt " <"; H.txt node_jid; H.txt ">"])
    in
    H.li [H.a ~a:[H.a_href (Vpaths.room node_jid)] label]
  in
  let ul = H.ul (List.map render_room rooms) in
  page ~title:"Room Index" [ul]

let () =
  Dream.run @@
  Dream.logger @@
  Dream.router [
    Dream.get Vpaths.index index;
    Dream.get (Vpaths.room ":room_jid") Room.handle;
    Dream.get (Vpaths.static "**") (Dream.static Config.(global.static_dir));
    Dream.post Protocol.count_messages_path Api.handle_count_messages;
    Dream.post Protocol.fetch_messages_path Api.handle_fetch_messages;
  ]
