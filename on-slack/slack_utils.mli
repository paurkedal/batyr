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

(* Incomplete, expecting this to be done properly in slacko. *)
type showable_error =
  [ Slacko.parsed_auth_error
  | Slacko.channel_error
  | Slacko.timestamp_error ]
val show_error : showable_error -> string

val demarkup : Slack_cache.t -> string -> string Lwt.t
