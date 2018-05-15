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

type error =
  [ Slacko.parsed_auth_error
  | Slacko.channel_error
  | Slacko.user_error
  | Slacko.user_visibility_error ]
(* TODO: show_error *)

module Make_cache (K : Hashtbl.HashedType) (V : Lru.Weighted) = struct
  include Lru.M.Make (K) (V)

  let memo cache f id : (_, error) result Lwt.t =
    (match find id cache with
     | None ->
        (match%lwt f id with
         | `Success obj ->
            add id obj cache;
            Lwt.return_ok obj
         | #error as r ->
            Lwt.return_error r)
     | Some v ->
        Lwt.return_ok v)
end

module Channel_hashable = struct
  type t = Slacko.channel
  let equal = (=)
  let hash = Hashtbl.hash
end
module Channel_weight = struct type t = Slacko.channel_obj let weight _ = 1 end
module Channel_cache = Make_cache (Channel_hashable) (Channel_weight)

module User_hashable = struct
  type t = Slacko.user
  let equal = (=)
  let hash = Hashtbl.hash
end
module User_weight = struct type t = Slacko.user_obj let weight _ = 1 end
module User_cache = Make_cache (User_hashable) (User_weight)

type t = {
  session: Slacko.session;
  channel_cache: Channel_cache.t;
  user_cache: User_cache.t;
}

let create ?(channel_cap = 50) ?(user_cap = 250) ~token () =
  let session = Slacko.start_session token in
  { session;
    channel_cache = Channel_cache.create channel_cap;
    user_cache = User_cache.create user_cap; }

let session cache = cache.session

let channel_obj_of_channel cache =
  Channel_cache.memo cache.channel_cache Slacko.(channels_info cache.session)

let user_obj_of_user cache =
  User_cache.memo cache.user_cache Slacko.(users_info cache.session)
