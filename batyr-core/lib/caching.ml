(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let section = Lwt_log.Section.make "batyr.cache"

let cache_hertz = Int64.to_float ExtUnix.Specific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz
let cache_metric =
  let current_time () =
    let tms = Unix.times () in
    Unix.(tms.tms_utime +. tms.tms_stime) in
  let current_memory_pressure =
    fun () -> cache_hertz (* 1 GHz / 1 Gword *) in
  let report cs =
    let open Prime_cache_metric in
    Lwt_log.ign_debug_f ~section
      "Beacon collection: time = %g; p = %g; n_live = %d; n_dead = %d"
      cs.cs_time cs.cs_memory_pressure
      cs.cs_live_count cs.cs_dead_count in
  Prime_cache_metric.create ~current_time ~current_memory_pressure ~report ()

module Beacon = Prime_beacon.Make (struct let cache_metric = cache_metric end)

module Grade = struct
  let basic = 1e-3 *. cache_second
  let basic_reduce n = basic *. float_of_int (n + 1)
  let from_size_and_cost size cost =
    basic +. float_of_int cost /. float_of_int (size + Beacon.overhead)
end

module type HASHABLE_WITH_BEACON = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val beacon : t -> Beacon.t
end

module type HASHED_CACHE = sig
  type data
  type t
  val create : int -> t
  val add : t -> data -> unit
  val remove : t -> data -> unit
  val merge : t -> data -> data
  val find : t -> data -> data
  val mem : t -> data -> bool
  val card : t -> int
  val iter : (data -> unit) -> t -> unit
  val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
end

module Cache_of_hashable (X : HASHABLE_WITH_BEACON) = struct
  include Weak.Make (X)
  let charge x = Beacon.charge (X.beacon x)
  let charged x = charge x; x
  let add ws x = if not (mem ws x) then (charge x; add ws x)
  let merge ws x = charged (merge ws x)
  let find ws x = charged (find ws x)
  let card = count
end

module type BIJECTION_WITH_BEACON = sig
  type domain
  type codomain
  val f : domain -> codomain
  val f_inv : codomain -> domain
  val beacon : domain -> Beacon.t
end

module type BIJECTION_CACHE = sig
  include HASHED_CACHE
  type key
  val merge_key : t -> key -> data
  val find_key : t -> key -> data
  val mem_key : t -> key -> bool
end

module Cache_of_bijection (X : BIJECTION_WITH_BEACON) = struct
  include Weak.Make
    (struct
      type t = X.domain
      let equal x0 x1 = X.f x0 = X.f x1
      let hash x = Hashtbl.hash (X.f x)
    end)
  type key = X.codomain
  let charge x = Beacon.charge (X.beacon x)
  let charged x = charge x; x
  let add ws x = if not (mem ws x) then (charge x; add ws x)
  let merge ws x = charged (merge ws x)
  let merge_key ws y = merge ws (X.f_inv y)
  let find ws x = charged (find ws x)
  let find_key ws y = find ws (X.f_inv y)
  let mem_key ws y = mem ws (X.f_inv y)
  let card = count
end

module Cache_of_physical_bijection (X : BIJECTION_WITH_BEACON) = struct
  include Weak.Make
    (struct
      type t = X.domain
      let equal x0 x1 = X.f x0 == X.f x1
      let hash x = Hashtbl.hash (X.f x)
    end)
  type key = X.codomain
  let charge x = Beacon.charge (X.beacon x)
  let charged x = charge x; x
  let add ws x = if not (mem ws x) then (charge x; add ws x)
  let merge ws x = charged (merge ws x)
  let merge_key ws y = merge ws (X.f_inv y)
  let find ws x = charged (find ws x)
  let find_key ws y = find ws (X.f_inv y)
  let mem_key ws y = mem ws (X.f_inv y)
  let card = count
end
