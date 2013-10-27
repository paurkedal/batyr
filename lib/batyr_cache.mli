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

type beacon

val dummy_beacon : beacon

val beacon_size : int

val cache : int -> (beacon -> 'a) -> 'a

val charge : beacon -> unit

module Grade : sig
  val basic : int
  val basic_reduce : int -> int
  val by_size_cost : int -> int -> int
end

module type CACHE_BIJECTION = sig
  type domain
  type codomain
  val f : domain -> codomain
  val f_inv : codomain -> domain
  val beacon : domain -> beacon
end

module type CACHE = sig
  type data
  type key
  type t
  val create : int -> t
  val add : t -> data -> unit
  val merge : t -> data -> data
  val merge_key : t -> key -> data
  val find : t -> data -> data
  val find_key : t -> key -> data
  val mem : t -> data -> bool
  val mem_key : t -> key -> bool
end

module Cache_of_bijection (X : CACHE_BIJECTION) :
  CACHE with type data = X.domain and type key = X.codomain

module Cache_of_physical_bijection (X : CACHE_BIJECTION) :
  CACHE with type data = X.domain and type key = X.codomain
