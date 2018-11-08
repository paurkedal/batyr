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

(** High-level access to database tables. *)

module Node : sig
  type t
  val create : domain_name: string -> ?node_name: string -> unit -> t
  val domain_name : t -> string
  val node_name : t -> string
  val to_string : t -> string

  val equal : t -> t -> bool
  val hash : t -> int

  val cached_of_id : int -> t option
  val cached_id : t -> int option
  val stored_of_id : int -> t Lwt.t
  val stored_id : t -> int option Lwt.t
  val store : t -> int Lwt.t
end

module Resource : sig
  type t
  val create : domain_name: string -> ?node_name: string ->
               ?resource_name: string -> unit -> t
  val create_on_node : Node.t -> string -> t

  val domain_name : t -> string
  val node_name : t -> string
  val resource_name : t -> string
  val node : t -> Node.t

  val equal : t -> t -> bool
  val hash : t -> int

  val cached_id : t -> int option
  val cached_of_id : int -> t option
  val stored_id : t -> int option Lwt.t
  val stored_of_id : int -> t Lwt.t
  val store : t -> int Lwt.t
end

module Account : sig
  type t
  val create : resource: Resource.t -> ?port: int ->
               password: string -> ?is_active: bool -> unit -> t Lwt.t
  val update : ?resource: Resource.t -> ?port: int -> ?password: string ->
               ?is_active: bool -> t -> unit Lwt.t
  val delete : t -> unit Lwt.t
  val delete_id : int -> unit Lwt.t
  val of_resource : Resource.t -> t option Lwt.t
  val all : unit -> t list Lwt.t
  val all_active : unit -> t list Lwt.t

  val resource : t -> Resource.t
  val host : t -> string
  val port : t -> int
  val password : t -> string
  val is_active : t -> bool

  val equal : t -> t -> bool
  val hash : t -> int
end

module Muc_room : sig
  type t
  val cached_of_node : Node.t -> t option
  val stored_of_node : Node.t -> t option Lwt.t
  val node : t -> Node.t
  val alias : t -> string option
  val description : t -> string option
  val transcribe : t -> bool
  val min_message_time : t -> float option
end

type message_type = [`Normal | `Chat | `Groupchat | `Headline]

val string_of_message_type : message_type -> string

(** High-level view of a row of the [messages] table. *)
module Message : sig

  type t

  val make :
    seen_time: Ptime.t ->
    ?edit_time: Ptime.t ->
    sender: Resource.t ->
    recipient: Resource.t ->
    message_type: message_type ->
    ?subject: string ->
    ?thread: string ->
    ?body: string ->
    unit -> t

  val seen_time : t -> Ptime.t
  val edit_time : t -> Ptime.t option
  val sender : t -> Resource.t
  val recipient : t -> Resource.t
  val message_type : t -> message_type
  val subject : t -> string option
  val thread : t -> string option
  val body : t -> string option

  val store : ?muc_author: Resource.t -> t ->
    (unit, [> Caqti_error.t]) result Lwt.t
end
