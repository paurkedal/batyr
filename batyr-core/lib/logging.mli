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

val lwt_reporter : unit -> Logs.reporter

module Verbosity : sig
  type t = {
    global: Logs.level option option;
    per_source: (string * Logs.level option) list;
  }

  val merge : t -> t -> t

  val of_string : string -> (t, [> `Msg of string]) result

  val cmdliner_term : t Cmdliner.Term.t
end

val setup : ?verbosity: Verbosity.t -> unit -> unit
