(* Copyright (C) 2018--2019  Petter A. Urkedal <paurkedal@gmail.com>
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
open Unprime_option

let connect uri = (module struct

  module Base = (val Batyr_data.connect uri)

  module Db = Base.Db

  module Node = struct
    include Base.Node

    let of_jid JID.{ldomain; lnode; lresource; _} =
      if lresource <> "" then
        invalid_arg "Batyr_data.Node.of_jid: Non-empty resource.";
      create ~domain_name:ldomain ~node_name:lnode ()

    let jid node = JID.make_jid (node_name node) (domain_name node) ""

    let to_string node_name = JID.string_of_jid (jid node_name)
    let of_string s = of_jid (JID.of_string s)
  end

  module Resource = struct
    include Base.Resource

    let of_jid JID.{ldomain; lnode; lresource; _} =
      create ~domain_name:ldomain ~node_name:lnode ~resource_name:lresource ()

    let jid resource =
      JID.make_jid (node_name resource) (domain_name resource)
                   (resource_name resource)

    let to_string p = JID.string_of_jid (jid p)
    let of_string s = of_jid (JID.of_string s)
  end

  module Account = Base.Account

  module Muc_user = struct
    type t = {
      nick : string;
      resource : Resource.t option;
      role : Chat_muc.role;
      affiliation : Chat_muc.affiliation
    }
    let make ~nick ?jid ~role ~affiliation () =
      let resource = Option.map Resource.of_jid jid in
      {nick; resource; role; affiliation}
    let nick {nick; _} = nick
    let jid {resource; _} = Option.map Resource.jid resource
    let resource {resource; _} = resource
    let role {role; _} = role
    let affiliation {affiliation; _} = affiliation
    let to_string = function
      | {nick; resource = None; _} -> nick
      | {nick; resource = Some resource; _} ->
        nick ^ " <" ^ Resource.to_string resource ^ ">"
  end

  module Muc_room = struct
    include Base.Muc_room
    let to_string room = Node.to_string (node room)
  end

  module Message = Base.Message

end : Data_sig.S)
