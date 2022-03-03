(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let src = Logs.Src.create "batyr-on-rocketchat"

include (val Logs_lwt.src_log src)

type level_option = Logs.level option

let level_option_of_yojson = function
 | `String level_name ->
    (match Logs.level_of_string level_name with
     | Ok level -> Ok level
     | Error (`Msg msg) -> Error msg)
 | _ -> Error "log level must be a string"

let level_option_to_yojson level = `String (Logs.level_to_string level)
