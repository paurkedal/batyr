(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Common

module H = Tyxml.Html

let page ?status ~title content =
  let head =
    H.head (H.title (H.txt title)) [
      H.link ~rel:[`Stylesheet] ~href:Vpaths.batyr_css ();
    ]
  in
  let body = H.body (H.h1 [H.txt title] :: content) in
  let html =
    H.html head body ~a:[H.a_user_data "batyr-root-vpath" (site_prefix ())]
  in
  Dream.html ?status (Format.asprintf "%a" (Tyxml.Html.pp ()) html)

let error_page ~status ~title message =
  page ~status ~title [H.p [H.txt message]]
