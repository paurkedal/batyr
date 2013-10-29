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

(** Caching based on Access Counts and Estimated Computation Cost.

    Instead of providing the cost directly, we are interrested in the cost per
    unit memory, since this is what matters when deciding whether or not to
    retain an object.  In the following we call this the grade of the object.
    It is provided upon construction with {!cache} and can be increased with
    {!enrich} in case more resources are used to enhance the object. *)

type beacon
(** The type of a field to embed in records in order to keep track of access
    and prevent actively used data to be garbage collected. *)

val dummy_beacon : beacon
(** A dummy {!beacon} object.  This can be usedful when creating temporary
    objects which only is used as a key to make lookups in a weak map. *)

val beacon_size : int
(** The size of a the beacon field. *)

val cache : int -> (beacon -> 'a) -> 'a
(** [cache grade f] passes a suitable beacon to [f], which is expected to
    construct an object which embeds the beacon, and return it.  Conversely
    the returned object is made accessible from the beacon which is kept
    visible to the garbage collector as long as the grade times the access
    frequency is above a certain threshold.  The access frequency is only a
    rought estimate, esp. until the first GC survival. *)

val enrich : int -> beacon -> unit
(** [enrich grade b] adds [grade] to the recorded grade of computing [b]. *)

val charge : beacon -> unit
(** [charge b] records the fact that [b] has been accessed.  This is typically
    called each time an object is acquired from a weak data structure.  The
    specialized weak hash tables below will do it for you. *)

(** Helper module to make grade estimates. *)
module Grade : sig

  val basic : int
  (** [basic] is used for a small constant-time sections of code. *)

  val basic_reduce : int -> int
  (** [basic_reduce n] is used for a small section of code which includes a
      linear-time loop of [n] iterations. *)

  val from_size_and_cost : int -> int -> int
  (** [from_size_and_cost n c] returns [basic + c / (n + beacon_size)]. *)
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
  (** [create n] creates a hash table of initial capacity [n].  For best
      performance [n] should be prime. *)

  val add : t -> data -> unit
  (** [add ht x] records an access of [x] and adds it to [ht] if it does not
      exist. *)

  val merge : t -> data -> data
  (** [merge ht x] adds [x] to [ht] if it does not exist, returns either the
      newly inserted element or an existing one, and records one access to
      it. *)

  val find : t -> data -> data
  (** [find ht x] returns an element of [ht] equal to [x] if such exists,
      and raises [Not_found] otherwise.  One access is recorded on the
      returned element. *)

  val mem : t -> data -> bool
  (** [mem ht x] is true iff an element equal to [x] is present in [ht].  No
      access is recorded. *)

  val card : t -> int
  (** [card ht] is the number of elements of [ht]. *)

  val iter : (data -> unit) -> t -> unit
  (** [iter f ht] calls [f] on each element of [ht]. *)

  val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f ht] forms the composition of [f x] for each [x] which is present
      in [ht] at the time it is applied. *)
end

(** A weak hash table based on the {!Weak} module from the standard
    library, which records access to the element's beacons. *)
module Cache_of_hashable (X : HASHABLE_WITH_BEACON) :
  HASHED_CACHE with type data = X.t

(** A bijection between two types plus access to the beacon of the former
    type.  This is used to translate between a member of a weak hash table and
    a unique key constructed from the member. *)
module type BIJECTION_WITH_BEACON = sig
  type domain
  type codomain

  val f : domain -> codomain
  (** An injective function from [domain] to [codomain]. *)

  val f_inv : codomain -> domain
  (** The inverse of {!f}. *)

  val beacon : domain -> beacon
  (** [beacon x] must return the beacon embedded in [x]. *)
end

(** The signature of a cache based on a weak hash table, using a bijection
    rather than a hashtable type.  In the following, let [X] denote the
    provided bijection. *)
module type BIJECTION_CACHE = sig
  include HASHED_CACHE

  type key

  val merge_key : t -> key -> data
  (** [merge_key y] returns [merge (X.f_inv y)]. *)

  val find_key : t -> key -> data
  (** [find_key y] returns [find (X.f_inv y)]. *)

  val mem_key : t -> key -> bool
  (** [mem_key y] returns [find (X.f_inv y)]. *)
end

(** Given a bijection between elements and unique keys, along with access to
    the beacons of the elements, this functor builds a weak hash table using
    structural equality of keys, and which records access to the beacons. *)
module Cache_of_bijection (X : BIJECTION_WITH_BEACON) :
  BIJECTION_CACHE with type data = X.domain and type key = X.codomain

(** Given a bijection between elements and unique keys, along with access to
    the beacons of the elements, this functor builds a weak hash table using
    physical equality of keys, and which records access to the beacons. *)
module Cache_of_physical_bijection (X : BIJECTION_WITH_BEACON) :
  BIJECTION_CACHE with type data = X.domain and type key = X.codomain
