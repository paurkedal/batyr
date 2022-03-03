(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Xmpp_inst

module type S = sig

  module Base : Batyr_data_sig.S

  module Db : Batyr_data_sig.Db

  module Node : sig
    include Batyr_data_sig.Node
      with type t = Base.Node.t

    val of_jid : JID.t -> t
    val jid : t -> JID.t
    val to_string : t -> string
    val of_string : string -> t
  end

  module Resource : sig
    include Batyr_data_sig.Resource
      with type node := Base.Node.t
       and type t = Base.Resource.t

    val of_jid : JID.t -> t
    val jid : t -> JID.t
    val to_string : t -> string
    val of_string : string -> t
  end

  module Account = Base.Account

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
    include Batyr_data_sig.Muc_room
      with type node := Base.Node.t
       and type t = Base.Muc_room.t

    val to_string : t -> string
  end

  module Message = Base.Message

end
