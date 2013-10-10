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

module JID = JID

module String_map = Map.Make (String)
module Chat = struct
  include XMPP.Make (Lwt) (Xmlstream.XmlStream) (String_map)
  type chat = unit session_data
end
type chat = unit Chat.session_data

let (>|=) = Lwt.(>|=)

let make_plain_socket fd =
  let module Socket = struct
    type t = Lwt_unix.file_descr
    let socket = fd
    let read fd buf start len = Lwt_unix.read fd buf start len
    let write fd buf = Lwt_unix.write fd buf 0 (String.length buf) >|= ignore
    let close fd = Lwt_unix.close fd
  end in
  Lwt.return (module Socket : Chat.Socket)

let make_tls_socket fd =
  Ssl.init ();
  let ssl_ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  lwt ssl_socket = Lwt_ssl.ssl_connect fd ssl_ctx in
  let module Socket = struct
    type t = Lwt_ssl.socket
    let socket = ssl_socket
    let read socket buf start len = Lwt_ssl.read socket buf start len
    let write socket buf =
      let n = String.length buf in
      let rec loop i =
	if i = n then Lwt.return_unit else
	lwt m = Lwt_ssl.write socket buf i (n - i) in
	assert (m > 0);
	loop (i + m) in
      loop 0
    let close socket = Lwt_ssl.close socket
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
  let myjid = JID.make_jid username server resource in
  let inetaddr =
    try Unix.inet_addr_of_string server
    with Failure _ -> (Unix.gethostbyname server).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inetaddr, port) in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd sockaddr >>
  lwt plain_socket = make_plain_socket fd in
  let tls_socket () = make_tls_socket fd in
  lwt session_data =
    Chat.setup_session ~user_data:() ~myjid ~plain_socket ~tls_socket
		       ~password session in
  lwt r = Chat.parse session_data in
  let module Socket = (val session_data.Chat.socket : Chat.Socket) in
  Socket.close Socket.socket

module Chat_muc = XEP_muc.Make (Chat)
module Chat_version = XEP_version.Make (Chat)
