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

open Lwt.Infix

type showable_error =
  [ Slacko.parsed_auth_error
  | Slacko.channel_error
  | Slacko.timestamp_error ]

let show_error = function
 | #Slacko.parsed_auth_error -> "parsed_auth_error"
 | #Slacko.channel_error -> "channel_error"
 | #Slacko.timestamp_error -> "timestamp_error"

module Message = struct

  type frag =
   | L of string
   | U of {userid: string; username: string option}
   | C of {channelid: string; channelname: string option}

  type t = frag list

  let to_string msg =
    let buf = Buffer.create 80 in
    let aux = function
     | L s -> Buffer.add_string buf s
     | U {userid; username = None} ->
        Buffer.add_char buf '@'; Buffer.add_string buf userid
     | U {userid; username = Some username} ->
        Buffer.add_char buf '@'; Buffer.add_string buf username
     | C {channelid; channelname = None} ->
        Buffer.add_char buf '#'; Buffer.add_string buf channelid
     | C {channelid; channelname = Some channelname} ->
        Buffer.add_char buf '#'; Buffer.add_string buf channelname
    in
    List.iter aux msg;
    Buffer.contents buf

  let resolve cache =
    let resolve_frag = function
     | L s -> Lwt.return (L s)
     | U {userid; username} as orig ->
        let user = Slacko.user_of_string userid in
        (match%lwt Slack_cache.user_obj_of_user cache user with
         | Ok user ->
            (match username with
             | Some username -> assert (username = user.Slacko.name)
             | None -> ());
            Lwt.return (U {userid; username = Some user.Slacko.name})
         | Error err ->
            (match username with
             | Some username ->
                Logs_lwt.warn (fun m ->
                  m "Failed to verify user id %s as %s." userid username)
             | None ->
                Logs_lwt.err (fun m -> m "Failed to expand user id %s." userid))
            >|= fun () -> orig)
     | C {channelid; channelname} as orig ->
        let channel = Slacko.channel_of_string channelid in
        (match%lwt Slack_cache.channel_obj_of_channel cache channel with
         | Ok channel ->
            (match channelname with
             | Some channelname -> assert (channelname = channel.Slacko.name)
             | None -> ());
            Lwt.return (C {channelid; channelname = Some channel.Slacko.name})
         | Error err ->
            (match channelname with
             | Some channelname ->
                Logs_lwt.warn (fun m ->
                  m "Failed to verify channel id %s as %s."
                    channelid channelname)
             | None ->
                Logs_lwt.err (fun m ->
                  m "Failed to expand channel id %s." channelid))
            >|= fun () -> orig)
    in
    Lwt_list.map_s resolve_frag

  let parse =
    let special_frag_re = Re.Pcre.regexp {|<([^<>]+)>|} in
    let entity_re = Re.Pcre.regexp {|&(amp|lt|gt);|} in
    let entity_repl g =
      (match Re.Group.get g 1 with
       | "amp" -> "&"
       | "lt" -> "<"
       | "gt" -> ">"
       | _ -> assert false) in
    let parse_frag = function
     | `Text s -> L (Re.replace entity_re ~f:entity_repl s)
     | `Delim g_frag ->
        let orig = Re.Group.get g_frag 0 in
        (match%pcre Re.Group.get g_frag 1 with
         | {|^(?<prot>https?://)(?<addr>[^|<>]+)\|(?<text>[^|<>]+)$|} ->
            (* Note trailing space added by Slack conversion to XMPP. *)
            L ((if addr = text then prot^addr else text^" "^prot^addr)^" ")
         | {|^(?<addr>https?://[^|<>]+)$|} ->
            L addr
         | {|^@(?<userid>U[0-9A-Z]+)$|} ->
            U {userid; username = None}
         | {|^@(?<userid>U[0-9A-Z]+)\|(?<username>[^<|>]+)$|} ->
            U {userid; username = Some username}
         | {|^!channel$|} ->
            L "@channel"
         | {|^#(?<channelid>C[0-9A-Z]+)\|(?<channelname>[^<|>]+)$|} ->
            C {channelid; channelname = Some channelname}
         | {|^mailto:(?<email>[^|]+)\|(?<label>[^|]+)|} ->
            if email = label then L email else
            (Logs.warn (fun m -> m "Email %S labelled %S." email label); L orig)
         | _ ->
            Logs.err (fun m -> m "Unrecognized markup %S." orig);
            L orig)
    in
    Re.split_full special_frag_re %> List.map parse_frag
end

let demarkup cache msg =
  let%lwt msg = Message.resolve cache (Message.parse msg) in
  Lwt.return (String.trim (Message.to_string msg))
