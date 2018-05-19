(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Concurrency, pooling, etc. for postgresql-ocaml using lwt. *)

module type CONNECTION = Caqti_lwt.CONNECTION

val escape_like : string -> string

val epoch_of_timestamp : string -> float
val timestamp_of_epoch : float -> string

type param = Param : 'a Caqti_type.t * 'a -> param

module Expr : sig
  type 'a t

  val to_sql : ?first_index: int -> 'a t -> string * param

  val of_sql : string -> 'a t
  val of_sql_f : ('b, unit, string, 'a t) format4 -> 'b

  val bool : bool -> bool t
  val int : int -> int t
  val float : float -> float t
  val string : string -> string t
  val string_f : ('a, unit, string, string t) format4 -> 'a
  val epoch : float -> [`timestamp] t
  val calendar : ?tz: string -> CalendarLib.Calendar.t -> [`timestamp] t
  val var : string -> 'a t
  val func1 : string -> 'a0 t -> 'r t
  val func2 : string -> 'a0 t -> 'a1 t -> 'r t

  val not : bool t -> bool t
  val (&&) : bool t -> bool t -> bool t
  val (||) : bool t -> bool t -> bool t
  val is_null : 'a t -> bool t
  val is_not_null : 'a t -> bool t
  val (=) : 'a t -> 'a t -> bool t
  val (<>) : 'a t -> 'a t -> bool t
  val (<=) : 'a t -> 'a t -> bool t
  val (>=) : 'a t -> 'a t -> bool t
  val (<) : 'a t -> 'a t -> bool t
  val (>) : 'a t -> 'a t -> bool t
  val (=~) : 'a t -> string -> bool t
  val ( =~* ) : 'a t -> string -> bool t
  val (=~@) : 'a t -> string -> bool t
  val like : 'a t -> string -> bool t
  val ilike : 'a t -> string -> bool t

  val (~-) : int t -> int t
  val (+) : int t -> int t -> int t
  val (-) : int t -> int t -> int t
  val ( * ) : int t -> int t -> int t
  val (/) : int t -> int t -> int t
  val (mod) : int t -> int t -> int t

  val (~-.) : float t -> float t
  val (+.) : float t -> float t -> float t
  val (-.) : float t -> float t -> float t
  val ( *. ) : float t -> float t -> float t
  val (/.) : float t -> float t -> float t

  val date_part : string -> [`timestamp] t -> int t
  val at_tz : string -> [`timestamp] t -> [`timestamp] t
end

val use : ?quick: bool ->
  ((module CONNECTION) -> ('a, ([> Caqti_error.connect]) as 'e) result Lwt.t) ->
  ('a, 'e) result Lwt.t

val use_exn : ?quick: bool ->
  ((module CONNECTION) -> ('a, [< Caqti_error.t]) result Lwt.t) -> 'a Lwt.t

val use_accounted : ?quick: bool ->
  ((module CONNECTION) -> ('a, ([> Caqti_error.connect]) as 'e) result Lwt.t) ->
  (float * 'a, 'e) result Lwt.t

val use_accounted_exn : ?quick: bool ->
  ((module CONNECTION) -> ('a, [< Caqti_error.t]) result Lwt.t) ->
  (float * 'a) Lwt.t
