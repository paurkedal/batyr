(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
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

let strip_markup_re =
  Re.compile @@ Re.Pcre.re "<(https?://)([^|<>]+)\\|([^|<>]+)>"

let strip_markup s =
  let f groups =
    let prot = Re.Group.get groups 1 in
    let addr = Re.Group.get groups 2 in
    let text = Re.Group.get groups 3 in
    (* Note trailing space added by Slack conversion to XMPP. *)
    if addr = text then prot ^ addr ^ " " else text ^ " " ^ prot ^ addr ^ " " in
  Re.replace ~f strip_markup_re s

let bare_url_re = Re.compile @@ Re.Perl.re "<(https?://[^|<>]+)>"
let bare_url_repl g = Re.Group.get g 1

let user_re = Re.compile @@ Re.Perl.re "<@(U[0-9A-Z]+)>"
let user_repl name_of_userid g = "@" ^ name_of_userid (Re.Group.get g 1)

let named_user_re = Re.compile @@ Re.Perl.re "<@(U[0-9A-Z]+)\\|([^<|>]+)>"
let named_user_repl g = "@" ^ Re.Group.get g 2

let channel_re = Re.compile @@ Re.Perl.re "<!channel>"
let channel_repl = "@channel"

let demarkup cache msg =
  let%lwt name_of_userid =
    let users = Hashtbl.create 7 in
    let add g =
      let id = Re.Group.get g 1 in
      (match%lwt Slack_cache.user_obj_of_id cache id with
       | Ok user_obj ->
          let name = user_obj.Slacko.name in
          Logs_lwt.debug (fun m -> m "Mapped %s to %s." id name) >|= fun () ->
          Hashtbl.add users id name
       | Error _ ->
          Logs_lwt.err (fun m -> m "Failed to look up %s." id) >|= fun () ->
          Hashtbl.add users id id)
    in
    Lwt_list.iter_s add (Re.all user_re msg) >|= fun () ->
    Hashtbl.find users in
  msg
   |> Re.replace user_re ~f:(user_repl name_of_userid)
   |> Re.replace named_user_re ~f:named_user_repl
   |> Re.replace_string channel_re ~by:channel_repl
   |> strip_markup
   |> Re.replace bare_url_re ~f:bare_url_repl
   |> String.trim
   |> Lwt.return
