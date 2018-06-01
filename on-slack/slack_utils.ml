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
  [ Slacko.api_error
  | Slacko.parsed_api_error
  | Slacko.auth_error
  | Slacko.timestamp_error
  | Slacko.channel_error
  | Slacko.user_error
  | Slacko.invite_error
  | Slacko.not_in_channel_error
  | Slacko.already_in_channel_error
  | Slacko.archive_error
  | Slacko.name_error
  | Slacko.kick_error
  | Slacko.channel_kick_error
  | Slacko.restriction_error
  | Slacko.leave_general_error
  | Slacko.message_error
  | Slacko.message_length_error
  | Slacko.attachments_error
  | Slacko.rate_error
  | Slacko.message_update_error
  | Slacko.file_error
  | Slacko.unknown_type_error
  | Slacko.already_archived_error
  | Slacko.not_in_group_error
  | Slacko.leave_last_channel_error
  | Slacko.last_member_error
  | Slacko.oauth_error
  | Slacko.presence_error
  | Slacko.user_visibility_error
  | Slacko.invalid_name_error
  | Slacko.bot_error
  | Slacko.parsed_auth_error ]

let show_error = function
 | `Unhandled_error msg -> "unhandled error: " ^ msg
 | `Unknown_error -> "unknown error"
 | `ParseFailure msg -> "parse failure: " ^ msg
 | `Not_authed -> "not authenticated"
 | `Invalid_auth -> "invalid authentication"
 | `Account_inactive -> "account inactive"
 | `Invalid_ts_latest -> "invalid latest timestamp"
 | `Invalid_ts_oldest -> "invalid oldest timestamp"
 | `Channel_not_found -> "channel not found"
 | `User_not_found -> "user not found"
 | `Cant_invite_self -> "can't invite self"
 | `Cant_invite -> "can't invite"
 | `Not_in_channel -> "not in channel"
 | `Already_in_channel -> "already in channel"
 | `Is_archived -> "is archived"
 | `Name_taken -> "name taken"
 | `Cant_kick_self -> "can't kick self"
 | `Cant_kick_from_general -> "can't kick from general"
 | `Cant_kick_from_last_channel -> "can't kick from last channel"
 | `Restricted_action -> "restricted access"
 | `Cant_leave_general -> "can't leave general"
 | `Cant_delete_message -> "can't delete message"
 | `Message_not_found -> "message not found"
 | `Msg_too_long -> "message too long"
 | `Too_many_attachments -> "too many attachments"
 | `Rate_limited -> "rate limited"
 | `Cant_update_message -> "can't update message"
 | `Edit_window_closed -> "edit window closed"
 | `File_not_found -> "file not found"
 | `File_deleted -> "file deleted"
 | `Unknown_type -> "unknown type"
 | `Already_archived -> "already archived"
 | `Not_in_group -> "not in group"
 | `Cant_leave_last_channel -> "can't leave last channel"
 | `Last_member -> "last member of channel"
 | `Invalid_client_id -> "invalid client ID"
 | `Bad_client_secret -> "bad client secret"
 | `Invalid_code -> "invalid code"
 | `Bad_redirect_uri -> "bad redirect URI"
 | `Invalid_presence -> "invalid presence"
 | `User_not_visible -> "user not visible"
 | `Invalid_name -> "invalid name"
 | `User_is_bot -> "user is bot"
 | `User_is_restricted -> "user is restricted"

let pp_error ppf error =
  Format.pp_print_string ppf (show_error error)

module Message = struct

  type frag =
   | L of string
   | U of {userid: string; username: string option}
   | C of {channelid: string; channelname: string option}

  [@@@ocaml.warning "-34"]
  type t = frag list
  [@@@ocaml.warning "+34"]

  let to_string msg =
    let buf = Buffer.create 80 in
    let aux = function
     | L s -> Buffer.add_string buf s
     | U {userid; username = None} ->
        Buffer.add_char buf '@'; Buffer.add_string buf userid
     | U {username = Some username; _} ->
        Buffer.add_char buf '@'; Buffer.add_string buf username
     | C {channelid; channelname = None} ->
        Buffer.add_char buf '#'; Buffer.add_string buf channelid
     | C {channelname = Some channelname; _} ->
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
                Logs_lwt.err (fun m -> m "Failed to expand user id %s: %s"
                  userid (show_error err)))
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
                  m "Failed to verify channel id %s as %s: %s"
                    channelid channelname (show_error err))
             | None ->
                Logs_lwt.err (fun m ->
                  m "Failed to expand channel id %s: %s"
                    channelid (show_error err)))
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
