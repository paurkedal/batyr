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

module Node : sig
  type t
  val create : domain: string -> ?node: string -> unit -> t
  val domain : t -> string
  val node : t -> string
  val of_jid : JID.t -> t
  val jid : t -> JID.t
  val to_string : t -> string
  val of_string : string -> t
  val of_id : int -> t Lwt.t
  val id : t -> int Lwt.t
end

module Peer : sig
  type t
  val create : domain: string -> ?node: string -> ?resource: string -> unit -> t
  val domain : t -> string
  val node : t -> string
  val resource : t -> string
  val of_jid : JID.t -> t
  val jid : t -> JID.t
  val to_string : t -> string
  val of_string : string -> t
  val of_id : int -> t Lwt.t
  val id : t -> int Lwt.t
end

module Muc_room : sig
  type t
  val of_node : Node.t -> t Lwt.t
  val node : t -> Node.t
  val alias : t -> string option
  val description : t -> string option
  val min_message_time : t -> float option
end
