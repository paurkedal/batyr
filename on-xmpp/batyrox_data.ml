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
open Unprime_option

module Node = struct
  include Batyr_data.Node

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    if lresource <> "" then
      invalid_arg "Batyr_data.Node.of_jid: Non-empty resource.";
    create ~domain_name:ldomain ~node_name:lnode ()

  let jid node = JID.make_jid (node_name node) (domain_name node) ""

  let to_string node_name = JID.string_of_jid (jid node_name)
  let of_string s = of_jid (JID.of_string s)
end

module Resource = struct
  include Batyr_data.Resource

  let of_jid {JID.ldomain; JID.lnode; JID.lresource} =
    create ~domain_name:ldomain ~node_name:lnode ~resource_name:lresource ()

  let jid resource =
    JID.make_jid (node_name resource) (domain_name resource)
                 (resource_name resource)

  let to_string p = JID.string_of_jid (jid p)
  let of_string s = of_jid (JID.of_string s)
end

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
  let nick {nick} = nick
  let jid {resource} = Option.map Resource.jid resource
  let resource {resource} = resource
  let role {role} = role
  let affiliation {affiliation} = affiliation
  let to_string = function
    | {nick; resource = None} -> nick
    | {nick; resource = Some resource} ->
      nick ^ " <" ^ Resource.to_string resource ^ ">"
end

module Muc_room = struct
  include Batyr_data.Muc_room
  let to_string room = Node.to_string (node room)
end
