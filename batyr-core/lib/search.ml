(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

include Search_types

let pattern_of_string = Search_lexer.parse_string

(* TODO: The common case, a text search in `Any field, can be optimised with a
 * dedicated tsvector column in messages. *)

let field_disj f = function
 | `Any -> Search_sql.Expr.(f "sender.resource_name" || f "subject" || f "body")
 | `Author -> f "sender.resource_name"
 | `Subject -> f "subject"
 | `Body -> f "body"

let rec denote_pattern pat =
  let open Search_sql.Expr in
  (match pat with
   | Sp_regex (field, s) ->
      field_disj (fun fn -> var fn =~* s) field
   | Sp_substring (field, s) ->
      field_disj (fun fn -> ilike (var fn) ("%" ^ s ^ "%")) field
   | Sp_word (field, s) ->
      field_disj (fun fn -> var fn =~@ s) field
   | Sp_not pat -> not (denote_pattern pat)
   | Sp_and (patA, patB) -> denote_pattern patA && denote_pattern patB
   | Sp_or (patA, patB) -> denote_pattern patA || denote_pattern patB)
