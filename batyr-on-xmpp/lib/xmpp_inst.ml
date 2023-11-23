(* Copyright (C) 2013--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Erm_xml
open Lwt.Infix
open Unprime_option

module Log = (val Logs_lwt.src_log (Logs.Src.create "batyr-on-xmpp"))

module JID = JID

module String_map = Map.Make (String)
module Chat = struct
  include XMPP.Make (Lwt) (Xmlstream.XmlStream) (String_map)
  type chat = unit session_data
end
type chat = unit Chat.session_data

let make_plain_socket fd =
  let module Socket = struct
    type t = Lwt_unix.file_descr
    let socket = fd
    let read fd buf start len = Lwt_unix.read fd buf start len
    let write fd buf =
      Lwt_unix.write_string fd buf 0 (String.length buf) >|= ignore
    let close fd = Lwt_unix.close fd
  end in
  Lwt.return (module Socket : Chat.Socket)

let ca_paths =
  (match Sys.getenv "BATYR_CA_PATH" with
   | s ->
      String.split_on_char ':' s
   | exception Not_found ->
      ["/etc/ssl/certs"; "/etc/pki/tls/certs/ca-bundle.crt"])

let load_authenticator () =
  let rec loop = function
   | fp :: fps ->
      (match%lwt Lwt_unix.stat fp with
       | {Unix.st_kind = S_REG; _} -> Lwt.return (`Ca_file fp)
       | {Unix.st_kind = S_DIR; _} -> Lwt.return (`Ca_dir fp)
       | _ -> loop fps
       | exception Unix.Unix_error _ -> loop fps)
   | _ ->
      Lwt.fail_with ("Cannot find CAs, searched " ^ String.concat ", " ca_paths)
  in
  loop ca_paths >>= X509_lwt.authenticator

let make_tls_socket host fd =
  Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna);
  let%lwt authenticator = load_authenticator () in
  let config = Tls.Config.client ~authenticator () in
  let%lwt tls_socket = Tls_lwt.Unix.client_of_fd config ~host fd in

  let module Socket = struct

    type t = Tls_lwt.Unix.t

    let socket = tls_socket

    let read socket buf start len =
      let cs = Cstruct.create len in
      let%lwt len' = Tls_lwt.Unix.read socket cs in
      for i = 0 to len' - 1 do
        Bytes.set buf (start + i) (Cstruct.get_char cs i)
      done;
      Log.debug (fun p -> p "In: [%s]" (Bytes.sub_string buf start len'))
        >>= fun () ->
      Lwt.return len'

    let write socket s =
      Tls_lwt.Unix.write socket (Cstruct.of_string s) >>= fun () ->
      Log.debug (fun p -> p "Out: [%s]" s)

    let close = Tls_lwt.Unix.close
  end in

  Lwt.return (module Socket : Chat.Socket)

module Chat_params = struct
  type t = {
    server : string;
    port : int;
    username : string;
    password : string;
    resource : string;
  }
  let make ~server ?(port = 5222)
           ~username ~password ?(resource = "xmpp3.0") () =
    {server; port; username; password; resource}
end

open Chat_params

let with_chat session {server; username; password; resource; port} =

  (* Connect fd to server *)
  let myjid = JID.make_jid username server resource in
  Log.info (fun p -> p "Connecting %s@%s/%s." username server resource)
    >>= fun () ->
  let inetaddr =
    try Unix.inet_addr_of_string server
    with Failure _ -> (Unix.gethostbyname server).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inetaddr, port) in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (try%lwt Lwt_unix.connect fd sockaddr with
   | exn -> Lwt_unix.close fd >>= fun () -> Lwt.fail exn) >>= fun () ->

  (* Create the session. *)
  let%lwt plain_socket = make_plain_socket fd in
  let tls_socket () =
    make_tls_socket Domain_name.(server |> of_string_exn |> host_exn) fd
  in
  let%lwt session_data =
    Chat.setup_session ~user_data:() ~myjid ~plain_socket ~tls_socket
                       ~password session in

  (* Run the session. *)
  (Chat.parse session_data)
  [%finally
    (* The XMPP library will replace this socket with the TLS version if
     * StartTLS was issued so this will close the full stack. *)
    let module Socket = (val session_data.Chat.socket : Chat.Socket) in
    Socket.close Socket.socket] >>= fun () ->

  Log.info (fun p -> p "Disconnected %s@%s/%s." username server resource)

module Chat_version = struct
  include XEP_version.Make (Chat)

  let on_version result req _jid_from _jid_to _lang () =
    match req with
    | Chat.IQGet _ -> result
    | Chat.IQSet _ -> Lwt.fail Chat.BadRequest

  let register
        ?(name = "Batyr (bot)")
        ?(version = Batyr_core.About.version_string)
        ?(os = Sys.os_type) chat =
    let el = encode {name; version; os} in
    let result = Lwt.return (Chat.IQResult (Some el)) in
    Chat.register_iq_request_handler chat ns_version (on_version result)
end

module Chat_disco = struct
  include XEP_disco.Make (Chat)

  let extract_features chat =
    let open Chat in
    [] |> IQRequestCallback.fold (fun ns _v acc -> Option.get ns :: acc)
                                 chat.iq_request
       |> StanzaHandler.fold (fun (ns, _) _v acc -> Option.get ns :: acc)
                             chat.stanza_handlers

  let register_info
      ?(category = "client") ?(type_ = "bot")
      ?(name = "Batyr") ?features chat =
    let on_disco _req jid_from _jid_to _lang () =
      let features =
        Option.get_else (fun () -> extract_features chat) features in
      match jid_from with
      | Some jid_from ->
        Log.info (fun p -> p "Received disco request from %s." jid_from)
          >>= fun () ->
        let els = make_disco_info ~category ~type_ ~name ~features () in
        let el = Xml.Xmlelement ((ns_disco_info, "query"), [], els) in
        Lwt.return (Chat.IQResult (Some el))
      | None ->
        Log.warn (fun p -> p "Failing disco request lacking from-field.")
          >>= fun () ->
        Lwt.fail Chat.BadRequest in
    Chat.register_iq_request_handler chat ns_disco_info on_disco
end

module Chat_ping = struct
  let ns_ping = Some "urn:xmpp:ping"

  let ping ~jid_from ~jid_to chat =
    let r = ref None in
    Chat.make_iq_request chat
      ~jid_from ~jid_to
      (Chat.IQGet Xml.(Xmlelement ((ns_ping, "ping"), [], [])))
      (fun resp _jid_from _jid_to _lang () ->
        Lwt.return @@
          match resp with
          | Chat.IQResult _ -> ()
          | Chat.IQError err -> r := Some err) >>= fun () ->
    Lwt.return !r

  let on_ping req jid_from _jid_to _lang () =
    match jid_from with
    | Some jid_from ->
      begin match req with
      | Chat.IQGet _ ->
        Log.info (fun p -> p "Received ping from %s." jid_from) >>= fun () ->
        Lwt.return (Chat.IQResult None)
      | Chat.IQSet _ ->
        Log.warn (fun p -> p "Failing ping set request from %s." jid_from)
          >>= fun () ->
        Lwt.fail Chat.BadRequest
      end
    | None ->
      Log.warn (fun p -> p "Failing ping request lacking from-field.")
        >>= fun () ->
      Lwt.fail Chat.BadRequest

  let register chat =
    Chat.register_iq_request_handler chat ns_ping on_ping
end

module Chat_muc = XEP_muc.Make (Chat)
