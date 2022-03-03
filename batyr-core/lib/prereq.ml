(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let (>>=?) = Lwt_result.Infix.( >>= )
let (>|=?) = Lwt_result.Infix.( >|= )
let (let*?) = Lwt_result.Syntax.( let* )
let (let+?) = Lwt_result.Syntax.( let+ )

module Lwt_result_list = struct

  let rec iter_s f = function
   | [] -> Lwt.return_ok ()
   | x :: xs -> f x >>=? (fun () -> iter_s f xs)

end

module Lwt_option = struct

  let map_s f = function None -> Lwt.return_none | Some x -> f x >|= Option.some

end
