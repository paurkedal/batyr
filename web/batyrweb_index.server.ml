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

open Batyrweb_server
open Eliom_content.Html

let index_handler () () =
  let%lwt rooms = Data.Db.use_exn Batyrweb_sql.Web.rooms in
  let render_room_link (node_id, domain_name, node_name, alias, transcribe) =
    let node_jid = node_name ^ "@" ^ domain_name in
    let label =
      match alias with
      | None ->
        [D.txt node_jid]
      | Some alias ->
        [D.txt alias; D.txt " <"; D.txt node_jid; D.txt ">"] in
    Lwt.return @@
      D.li [D.a ~service:transcript_service label (node_jid, None)] in
  let%lwt room_lis = Lwt_list.map_p render_room_link rooms in
  let rooms_ul = D.ul room_lis in
  Lwt.return (Batyrweb_content.page "Chatrooms" [rooms_ul])

let () = Main_app.register ~service:index_service index_handler
