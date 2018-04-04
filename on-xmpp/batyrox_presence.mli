(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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
open Batyrox_xmpp

exception Session_shutdown

val messages : Message.t Lwt_react.E.t
(** This emits an events as new message are detected. *)

module Session : sig
  type t

  val start : Account.t -> t
  (** [persist account] tries to maintain an active session logged in to
      [account] and joining the rooms as described by the [muc_presence]
      table. *)

  val start_all : unit -> unit Lwt.t
  (** Log in to accounts and join chats according to the active entries in the
      [accounts] and [muc_presence] tables. *)

  val find : Account.t -> t option
  (** [find account] is the session handle of [account] if it has been started
      and not shut down yet. *)

  val is_active : t -> bool
  (** [is_active session] is true if [session] is currently logged in to the
      associated account. *)

  val with_chat : (Chat.chat -> 'a Lwt.t) -> t -> 'a Lwt.t
  (** [with_chat f session] calls [f] with the XMPP session handle when it
      becomes available.
      @raise Session_shutdown if {!shutdown} has been called on [session]. *)

  val shutdown : t -> unit Lwt.t
  (** [shutdown session] terminates the XMPP stream and closes resources
      associated with [session]. *)

end
