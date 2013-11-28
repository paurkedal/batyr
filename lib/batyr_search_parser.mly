/* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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
 */

%{
open Batyr_search_types
%}

%token<Batyr_search_types.search_pattern> ATOM
%token LPAREN RPAREN OR NOT EOF

%type<Batyr_search_types.search_pattern> start
%start start
%%
start: disj EOF { $1 };
disj:
    conj { $1 }
  | disj OR conj { Sp_or ($1, $3) }
  ;
conj:
    atom { $1 }
  | conj atom { Sp_and ($1, $2) }
  ;
atom:
    ATOM { $1 }
  | NOT atom { Sp_not $2 }
  | LPAREN disj RPAREN { $2 }
  ;
