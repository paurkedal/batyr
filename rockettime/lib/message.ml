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
module Decode = Decoders_yojson.Basic.Decode

type attachment = {
  ts: Ptime.t;
  text: string option;
  (* many more fields *)
}

let attachment_decoder =
  let open Decode in
  let* ts = field "ts" ptime_decoder in
  let+ text = field_opt "text" string in
  {ts; text}

type url = {
  url: string;
  headers: (string * string) list;
  meta: (string * string) list;
  (* parlsedUrl *)
}

let url_decoder =
  let open Decode in
  let* url = field "url" string in
  let* headers = field_opt "headers" (key_value_pairs string) in
  let+ meta = field_opt "meta" (key_value_pairs string) in
  { url; headers = Option.value ~default:[] headers;
    meta = Option.value ~default:[] meta }

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

let decoder =
  let open Decode in
  let* id = field "_id" string in
  let* rid = field "rid" string in
  let* msg = field "msg" string in
  let* ts = field "ts" ptime_decoder in
  let* u = field "u" User.decoder in
  let* updated_at = field "_updatedAt" ptime_decoder in
  let* edited_at = field_opt "editedAt" ptime_decoder in
  let* edited_by = field_opt "editedBy" User.decoder in
  let* urls = field_opt "urls" (list url_decoder) in
  let* attachments = field_opt "attachments" (list attachment_decoder) in
  let* alias = field_opt "alias" string in
  let* avatar = field_opt "avatar" string in
  let* groupable = field_opt "groupable" bool in
  let+ parse_urls = field_opt "parseUrls" bool in
  let urls = Option.value ~default:[] urls in
  let attachments = Option.value ~default:[] attachments in
  { id; rid; msg; ts; u; updated_at; edited_at; edited_by;
    urls; attachments; alias; avatar; groupable; parse_urls }
