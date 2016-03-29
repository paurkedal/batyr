(* Copyright (C) 2013--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Search patterns for logged messages (types). *)

type search_field = [`Any | `Author | `Subject | `Body]
type search_op = [`Regex | `Substring | `Word]

(** The compiled pattern type. *)
type search_pattern =
  | Sp_regex of search_field * string
  | Sp_substring of search_field * string
  | Sp_word of search_field * string
  | Sp_not of search_pattern
  | Sp_and of search_pattern * search_pattern
  | Sp_or of search_pattern * search_pattern

exception Syntax_error of string
(** Error raised by the pattern compiler. *)
