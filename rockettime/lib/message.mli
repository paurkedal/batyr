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

open Types

type attachment = {
  ts: Ptime.t option;
  text: string option;
  (* many more fields *)
}

type url = {
  url: string;
  headers: (string * string) list;
  meta: (string * string) list;
  (* parlsedUrl *)
}

type t = {
  id: string;
  rid: string;
  msg: string;
  ts: Ptime.t;
  u: User.t;
  updated_at: Ptime.t;
  edited_at: Ptime.t option;
  edited_by: User.t option;
  urls: url list;
  attachments: attachment list;
  alias: string option;
  avatar: string option;
  groupable: bool option;
  parse_urls: bool option;
}

val decoder : t decoder
