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

val show_error : [< showable_error] -> string

val pp_error : Format.formatter -> [< showable_error] -> unit

val demarkup : Slack_cache.t -> string -> string Lwt.t
