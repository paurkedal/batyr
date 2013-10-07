(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

exception Response_error of string

val or_null : string option -> string

module Decode : sig
  type 'a t

  val ( * ) : 'a t -> 'b t -> ('a * 'b) t

  val string : string t
  val bool : bool t
  val int : int t
  val float : float t

  val call : 'a t -> string array -> 'a
end

class conn :
  ?host: string -> ?hostaddr: string -> ?port: string  ->
  ?dbname: string -> ?user: string -> ?password: string ->
  ?options: string -> ?tty: string -> ?requiressl: string ->
  ?conninfo: string -> unit ->
object
  inherit Postgresql.connection

  method fetch_result : Postgresql.result option Lwt.t

  method fetch_last_result : Postgresql.result Lwt.t

  method query :
    ?params: string array -> ?binary_params: bool array -> string ->
    Postgresql.result Lwt.t

  method command :
    ?params: string array -> ?binary_params: bool array -> string ->
    unit Lwt.t

  method query_single : 'a. 'a Decode.t ->
    ?params: string array -> ?binary_params: bool array -> string ->
    'a Lwt.t

  method query_option : 'a. 'a Decode.t ->
    ?params: string array -> ?binary_params: bool array -> string ->
    'a option Lwt.t

  method query_array : 'a. 'a Decode.t ->
    ?params: string array -> ?binary_params: bool array -> string ->
    'a array Lwt.t

  method query_list : 'a. 'a Decode.t ->
    ?params: string array -> ?binary_params: bool array -> string ->
    'a list Lwt.t
end
