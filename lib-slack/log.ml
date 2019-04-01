(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
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

include Protocol_conv_jsonm

let src = Logs.Src.create "batyr-on-slack"

include (val Logs_lwt.src_log src)

type level_option = Logs.level option

let level_option_of_jsonm = function
 | `String level_name ->
    (match Logs.level_of_string level_name with
     | Ok level -> level
     | Error (`Msg msg) ->
        raise (Jsonm.Protocol_error (msg, `String level_name)))
 | json -> raise (Jsonm.Protocol_error ("invalid log level", json))

let level_option_to_jsonm level = `String (Logs.level_to_string level)
