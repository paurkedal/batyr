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

(** Caching based on Access Counts and Estimated Computation Cost. *)

type beacon
(** The type of a field to embed in records in order to keep track of access
    and prevent actively used data to be garbage collected. *)

val dummy_beacon : beacon
(** A dummy {!beacon} object.  This can be usedful when creating temporary
    objects which only is used as a key to make lookups in a weak map. *)

val beacon_size : int
(** The size of a the beacon field. *)

val cache : int -> (beacon -> 'a) -> 'a
(** [cache cost f] passes a suitable beacon to [f], which is expected to
    construct an object which embeds the beacon, and return it.  Conversely
    the returned object is made accessible from the beacon which is kept
    visible to the garbage collector as long as the cost times the access
    frequency is above a certain threshold.  The access frequency is only a
    rought estimate, esp. until the first GC survival. *)

val enrich : int -> beacon -> unit
(** [enrich cost b] adds [cost] to the recorded cost of computing [b]. *)

val charge : beacon -> unit
(** [charge b] records the fact that [b] has been accessed.  This is typically
    called each time an object is acquired from a weak data structure.  The
    specialized weak hash tables below will do it for you. *)

(** Helper module to make cost estimates. *)
module Grade : sig
  val basic : int
  val basic_reduce : int -> int
  val by_size_cost : int -> int -> int
end

(** The {!Hashtbl.HashedType} plus access to the object's beacon. *)
module type HASHABLE_WITH_BEACON = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val beacon : t -> beacon
end

(** Signature for a weak hash table. *)
module type HASHED_CACHE = sig
  type data
  type t
  val create : int -> t
  val add : t -> data -> unit
  val merge : t -> data -> data
  val find : t -> data -> data
  val mem : t -> data -> bool
  val card : t -> int
  val iter : (data -> unit) -> t -> unit
  val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
end

(** A weak hash table based on the {!Weak} module from the standard
    library. *)
module Cache_of_hashable (X : HASHABLE_WITH_BEACON) :
  HASHED_CACHE with type data = X.t

(** A bijection between two types plus access to the beacon of the former
    type.  This is used to translate between a member of a weak hash table and
    a unique key constructed from the member. *)
module type BIJECTION_WITH_BEACON = sig
  type domain
  type codomain
  val f : domain -> codomain
  val f_inv : codomain -> domain
  val beacon : domain -> beacon
end

(** The type of a weak cache based on hash tables, using a bijection rather
    than a hashtable type. *)
module type BIJECTION_CACHE = sig
  include HASHED_CACHE
  type key
  val merge_key : t -> key -> data
  val find_key : t -> key -> data
  val mem_key : t -> key -> bool
end

(** A weak hash table implemented by a bijection between the element and a
    unique key identifying it.  This variant uses structural equality to
    compare the keys. *)
module Cache_of_bijection (X : BIJECTION_WITH_BEACON) :
  BIJECTION_CACHE with type data = X.domain and type key = X.codomain

(** A weak hash table implemented by a bijection between the element and a
    unique key identifying it.  This variant uses physical equality to compare
    the keys. *)
module Cache_of_physical_bijection (X : BIJECTION_WITH_BEACON) :
  BIJECTION_CACHE with type data = X.domain and type key = X.codomain
