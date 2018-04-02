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

(** Cached fetching of Slack objects. *)

type error

type t

val create :
  ?channel_cap: int ->
  ?user_cap: int ->
  token: string -> unit -> t

val channel_obj_of_id : t -> string -> (Slacko.channel_obj, error) result Lwt.t

val user_obj_of_id : t -> string -> (Slacko.user_obj, error) result Lwt.t
