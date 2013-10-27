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

let section = Lwt_log.Section.make "batyr.cache"

type beacon = {
  mutable owner : Obj.t;
  mutable next_beacon : beacon;
  mutable cur_access_count : int; (* -1 if not linked *)
  mutable avg_access_count : int; (* -1 until first GC *)
  grade : int;
}

let rec head_beacon = {
  owner = Obj.repr "head_beacon";
  next_beacon = head_beacon;
  cur_access_count = -1;
  avg_access_count = -1;
  grade = -1;
}

let dummy_beacon = {
  owner = Obj.repr "dummy_beacon";
  next_beacon = head_beacon;
  cur_access_count = -2;
  avg_access_count = -2;
  grade = 0;
}

let beacon_size = Obj.size (Obj.repr head_beacon)

let cache_threshold = ref 10000

let check_beacon b =
  if b.avg_access_count = -1 then (* first GC survival *)
    b.avg_access_count <- 2 * b.cur_access_count
  else
    b.avg_access_count <- (b.avg_access_count + b.cur_access_count) / 2;
  b.grade * b.avg_access_count > !cache_threshold

let discard_depleted_beacons () =
  Lwt_log.ign_debug ~section "Starting cache cleanup.";
  let rec loop b nL nD =
    if b.next_beacon.grade = -1 then (nL, nD) else
    if check_beacon b.next_beacon then begin
      b.next_beacon.cur_access_count <- 0;
      loop b.next_beacon (nL + 1) nD
    end else begin
      b.next_beacon.cur_access_count <- -1;
      b.next_beacon <- b.next_beacon.next_beacon;
      loop b nL (nD + 1)
    end in
  let nL, nD = loop head_beacon 0 0 in
  Lwt_log.ign_debug_f ~section "Kept %d and removed %d entries." nL nD

let beacon_alarm = Gc.create_alarm discard_depleted_beacons

let cache g f =
  let b = {
    owner = Obj.repr head_beacon;
    next_beacon = head_beacon;
    cur_access_count = -1;
    avg_access_count = -1;
    grade = g;
  } in
  let obj = f b in
  b.owner <- Obj.repr obj; obj

let charge b =
  assert (b != dummy_beacon);
  if b.cur_access_count = -1 then begin
    b.next_beacon <- head_beacon.next_beacon;
    head_beacon.next_beacon <- b;
    b.cur_access_count <- 1
  end else
    b.cur_access_count <- b.cur_access_count + 1

let charged b = charge b; b

module Grade = struct
  let basic = 50
  let basic_reduce n = 50 * (n + 1)
  let by_size_cost size cost = basic + cost / (size + beacon_size)
end

module type HASHABLE_WITH_BEACON = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val beacon : t -> beacon
end

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

module Cache_of_hashable (X : HASHABLE_WITH_BEACON) = struct
  include Weak.Make (X)
  let charge x = charge (X.beacon x)
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
  val beacon : domain -> beacon
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
  let charge x = charge (X.beacon x)
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
  let charge x = charge (X.beacon x)
  let charged x = charge x; x
  let add ws x = if not (mem ws x) then (charge x; add ws x)
  let merge ws x = charged (merge ws x)
  let merge_key ws y = merge ws (X.f_inv y)
  let find ws x = charged (find ws x)
  let find_key ws y = find ws (X.f_inv y)
  let mem_key ws y = mem ws (X.f_inv y)
  let card = count
end
