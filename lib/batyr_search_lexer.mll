(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

{
open Batyr_search_parser
open Batyr_search_types
}

let wordbound = ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\128'-'\255']
let wordchar =  ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\128'-'\255' '.' '-']

rule lex = parse
  | [' ' '\t']+ { lex lexbuf }
  | '\n' { Lexing.new_line lexbuf; lex lexbuf }
  | '/' { ATOM (Sp_regex (lex_regex (Buffer.create 64) lexbuf)) }
  | '"' { ATOM (Sp_substring (lex_quoted (Buffer.create 64) lexbuf)) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '|' { OR }
  | '!' { NOT }
  | wordbound (wordchar* wordbound)? as s { ATOM (Sp_word s) }
  | eof { EOF }

and lex_quoted buf = parse
  | '"' { Buffer.contents buf }
  | '\\' (_ as c) { Buffer.add_char buf c; lex_quoted buf lexbuf }
  | [^ '\\' '"']+ as s { Buffer.add_string buf s; lex_quoted buf lexbuf }

and lex_regex buf = parse
  | '/' { Buffer.contents buf }
  | '\\' (_ as c) { Buffer.add_char buf c; lex_regex buf lexbuf }
  | [^ '\\' '/']+ as s { Buffer.add_string buf s; lex_regex buf lexbuf }

{
  open Lexing

  let start' lex lexbuf =
    try start lex lexbuf with
    | Failure msg -> raise (Syntax_error msg)
    | Parsing.Parse_error -> raise (Syntax_error "syntax error")

  let parse_file path =
    let ic = open_in path in
    let lexbuf = from_channel ic in
    lexbuf.lex_curr_p <- {
      pos_fname = path;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    };
    try start' lex lexbuf with
    | xc -> close_in ic; raise xc

  let parse_string str =
    let lexbuf = from_string str in
    lexbuf.lex_curr_p <- {
      pos_fname = "*string*";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    };
    start' lex lexbuf
}
