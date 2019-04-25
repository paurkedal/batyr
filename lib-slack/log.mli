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

open Protocol_conv_jsonm

val src : Logs.Src.t

include Logs_lwt.LOG

type level_option = Logs.level option
[@@deriving protocol ~driver:(module Jsonm)]

val jsonm_errorf : Jsonm.t -> ('a, unit, string, 'b) format4 -> 'a
