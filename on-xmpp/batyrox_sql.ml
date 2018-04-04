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

module type CONNECTION = Caqti_lwt.CONNECTION

module Caqti_type = struct
  include Caqti_type
  include Caqti_type_calendar
end

module Presence = struct
  let room_presence_q = Caqti_request.collect
    Caqti_type.int
    Caqti_type.(tup3 int (option string) (option ctime))
    "SELECT resource_id, nick, \
            (SELECT max(seen_time) \
               FROM batyr.messages JOIN batyr.resources AS sender \
                 ON sender_id = sender.resource_id \
             WHERE sender.node_id = node_id) \
     FROM batyr.muc_presence NATURAL JOIN batyr.resources \
     WHERE account_id = ? AND is_present = true"
  let room_presence account_id (module C : CONNECTION) =
    C.fold room_presence_q List.cons account_id []

  let active_accounts_q = Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup3 int int string)
    "SELECT resource_id, server_port, client_password \
     FROM batyr.accounts NATURAL JOIN batyr.resources \
     WHERE is_active = true"
  let active_accounts (module C : CONNECTION) =
    C.fold active_accounts_q List.cons () []
end
