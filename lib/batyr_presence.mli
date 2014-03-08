(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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

(** XMPP presence management and logger. *)

open Batyr_data
open Batyr_xmpp

(** High-level view of a row of the [messages] table. *)
module Message : sig
  type t

  val seen_time : t -> float
  val sender : t -> Resource.t
  val recipient : t -> Resource.t
  val message_type : t -> Chat.message_type
  val subject : t -> string option
  val thread : t -> string option
  val body : t -> string option
end

val message_events : Message.t Lwt_react.E.t
(** This emits an events as new message are detected. *)

val start_chat_sessions : unit -> unit Lwt.t
(** Log in to accounts and join chats according to the active entries in the
    [accounts] and [muc_presence] tables. *)
