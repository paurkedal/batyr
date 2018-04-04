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

open Batyrox_xmpp

module Node : sig
  include module type of Batyr_data.Node with type t = Batyr_data.Node.t
  val of_jid : JID.t -> t
  val jid : t -> JID.t
  val to_string : t -> string
  val of_string : string -> t
end

module Resource : sig
  include module type of Batyr_data.Resource with type t = Batyr_data.Resource.t
  val of_jid : JID.t -> t
  val jid : t -> JID.t
  val to_string : t -> string
  val of_string : string -> t
end

module Muc_user : sig
  type t
  val make : nick: string -> ?jid: JID.t -> role: Chat_muc.role ->
             affiliation: Chat_muc.affiliation -> unit -> t
  val nick : t -> string
  val jid : t -> JID.t option
  val resource : t -> Resource.t option
  val role : t -> Chat_muc.role
  val affiliation : t -> Chat_muc.affiliation
  val to_string : t -> string
end

module Muc_room : sig
  include module type of Batyr_data.Muc_room with type t = Batyr_data.Muc_room.t
  val to_string : t -> string
end
