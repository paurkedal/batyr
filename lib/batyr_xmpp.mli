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

(** XMPP based on erm-xmpp specialized for Lwt. *)

(** {2 Basic Session} *)

module JID : module type of JID with type t = JID.t

module Chat : sig

  include XMPP.S with type 'a t = 'a Lwt.t

  type chat = unit session_data

  val ns_streams : string option
  val ns_server : string option
  val ns_client : string option
  val ns_xmpp_tls : string option
  val ns_xmpp_sasl : string option
  val ns_xmpp_bind : string option
  val ns_xmpp_session : string option

  val register_iq_request_handler :
	chat -> Xml.namespace ->
	(iq_request -> string option -> string option -> string option ->
	 unit -> iq_response Lwt.t) ->
	unit

  val register_stanza_handler :
	chat -> Xml.qname ->
	(chat -> Xml.attribute list -> Xml.element list -> unit Lwt.t) ->
	unit

  val parse_message :
	callback: (chat -> message_content stanza -> 'a) ->
	callback_error: (chat -> ?id: string ->
			 ?jid_from: JID.t -> ?jid_to: string -> ?lang: string ->
			 StanzaError.t -> 'a) ->
	chat -> Xml.attribute list -> Xml.element list -> 'a
end

type chat = unit Chat.session_data

module Chat_params : sig

  type t = {
    server : string;
    port : int;
    username : string;
    password : string;
    resource : string;
  }

  val make :
	server: string -> ?port : int ->
	username: string -> password: string -> ?resource : string ->
	unit -> t
end

val with_chat : (chat -> unit Lwt.t) -> Chat_params.t -> unit Lwt.t


(** {2 Features} *)

module Chat_version : sig

  type t = {
    name : string;
    version : string;
    os : string;
  }

  val ns_version : Xml.namespace

  val encode : t -> Xml.element

  val decode : Xml.attribute list -> Xml.element list -> t option

  val get : chat -> ?jid_from: JID.t -> ?jid_to: JID.t -> ?lang: string ->
	    ?error_callback: (StanzaError.t -> unit Lwt.t) ->
	    (?jid_from: JID.t -> ?jid_to: JID.t -> ?lang: string ->
	     t option -> unit Lwt.t) ->
	    unit Lwt.t

  val iq_request :
	get: (?jid_from: JID.t -> ?jid_to: JID.t -> ?lang: string ->
	      unit -> t) ->
	Chat.iq_request -> JID.t option -> JID.t option -> string option ->
	unit -> Chat.iq_response

  val register : ?name: string -> ?version: string -> ?os: string ->
		 Chat.chat -> unit
end

module Chat_disco : sig
  val ns_disco_info : string option
  val ns_disco_items : string option
  val register_info : ?category: string -> ?type_: string -> ?name: string ->
		      ?features: string list -> Chat.chat -> unit
end

module Chat_ping : sig
  val ns_ping : string option
  val register : Chat.chat -> unit
end

module Chat_muc : sig

  val ns_muc : string option
  val ns_muc_user : string option
  val ns_muc_admin : string option
  val ns_muc_owner : string option
  val ns_muc_unique : string option

  type role =
    | RoleModerator
    | RoleParticipant
    | RoleVisitor
    | RoleNone

  type affiliation =
    | AffiliationOwner
    | AffiliationAdmin
    | AffiliationMember
    | AffiliationOutcast
    | AffiliationNone

  type muc_data = {
    maxchars : int option;
    maxstanzas : int option;
    seconds : int option;
    since : int option;
    password : string option;
  }
  val encode_muc : ?maxchars: int -> ?maxstanzas: int -> ?seconds: int ->
		   ?since: int -> ?password: string -> unit -> Xml.element
  val decode_muc : Xml.element -> muc_data

  module User : sig
    type item = {
      actor : JID.t option;
      continue : string option;
      reason : string option;
      jid : JID.t option;
      nick : string option;
      affiliation : affiliation option;
      role : role option;
    }
    type  data = {
      decline : (JID.t option * JID.t option * string option) option;
      destroy : (JID.t option * string option) option;
      invite : (JID.t option * JID.t option * string option) list;
      item : item option;
      password : string option;
      status : int list;
    }
    val encode : data -> Xml.element
    val decode : Xml.element -> data
  end

  module Admin : sig
    type item = {
      actor : JID.t option;
      reason : string option;
      jid : JID.t option;
      nick : string option;
      affiliation : affiliation option;
      role : role option;
    }
    val encode : Xml.element list -> Xml.element
    val encode_item : ?actor: JID.t -> ?reason: string ->
		      ?affiliation: affiliation -> ?jid: string ->
		      ?nick: string -> ?role: role -> unit -> Xml.element
    val decode : Xml.element -> item list
  end

  module Owner : sig
    val encode_destroy : ?jid: string -> ?password: string -> ?reason: string ->
			 unit -> Xml.element
    val decode_destroy : Xml.element ->
			 string option * string option * string option
    val encode_xdata : Xml.element -> Xml.element
    val decode_xdata : Xml.element -> unit
  end

  val enter_room :
	chat ->
	?maxchars: int ->
	?maxstanzas: int ->
	?seconds: int ->
	?since: int ->
	?password: string ->
	?nick: string ->
	JID.t -> unit Lwt.t

  val leave_room :
	chat ->
	?reason: string ->
	nick: string ->
	JID.t -> unit Lwt.t
end
