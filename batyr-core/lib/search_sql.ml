(* Copyright (C) 2013--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf
open Unprime_list
open Unprime_string

let escape_like s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if c = '\\' || c = '%' || c = '_' then Buffer.add_char buf '\\';
      Buffer.add_char buf c)
    s;
  Buffer.contents buf

let timestamp_of_epoch x =
  (match Ptime.of_float_s x with
   | None -> failwith "Ptime.of_float_s"
   | Some t -> Ptime.to_rfc3339 ~tz_offset_s:0 t)

type param = Param : 'a Caqti_type.t * 'a -> param

module Expr = struct

  type op =
    | Func of string
    | Prefix of string * int * int
    | Suffix of string * int * int
    | Infix of string * int * int list

  type u =
    | Literal of string
    | String of string
    | Var of string
    | Call of op * u list

  type 'a t = u

  let p_comma = 1

  let paren_if cond q = if cond then Caqti_query.(S[L"("; q; L")"]) else q

  let rec to_sql' prec =
    let open Caqti_query in
    (function
     | Literal s -> L s
     | String s -> Q s
     | Var v -> L v
     | Call (Func name, args) ->
        S[L name; L"("; concat ", " (List.map (to_sql' p_comma) args)]
     | Call (Prefix (name, p, p0), [arg]) ->
        paren_if (prec > p) (S[L name; L" "; to_sql' p0 arg])
     | Call (Suffix (name, p, p0), [arg]) ->
        paren_if (prec > p) (S[to_sql' p0 arg; L" "; L name])
     | Call ((Prefix _ | Suffix _), _) -> assert false
     | Call (Infix (name, p, ps), args) ->
        paren_if (prec > p) (concat (" "^name^" ") (List.map2 to_sql' ps args)))

  let to_sql e = to_sql' 0 e

  let prefix prec name = Prefix (name, prec, prec)
  let suffix prec name = Suffix (name, prec, prec)
  let infixA prec name = Infix (name, prec, [prec; prec])
  let infixB prec name = Infix (name, prec, [prec + 1; prec + 1])
  let infixL prec name = Infix (name, prec, [prec; prec + 1])
  let call1 op e0 = Call (op, [e0])
  let call2 op e0 e1 = Call (op, [e0; e1])
  let func1 name e0 = Call (Func name, [e0])
  let func2 name e0 e1 = Call (Func name, [e0; e1])

  let of_sql x = Literal x
  let of_sql_f fmt = ksprintf (fun s -> Literal s) fmt
  let bool x = Literal (string_of_bool x)
  let int x = Literal (string_of_int x)
  let float x = Literal (string_of_float x)
  let string x = String x
  let string_f fmt = ksprintf (fun s -> String s) fmt
  let epoch x = String (timestamp_of_epoch x)
  let var v = Var v

  let not       = let op = prefix 12 "not" in call1 op
  let (&&)      = let op = infixA 11 "and" in call2 op
  let (||)      = let op = infixA 10 "or" in call2 op
  let is_null   = let op = suffix 18 "is null" in call1 op
  let is_not_null=let op = suffix 18 "is not null" in call1 op
  let (=)       = let op = infixB 15 "=" in call2 op
  let (<>)      = let op = infixB 15 "!=" in call2 op
  let (<=)      = let op = infixB 15 "<=" in call2 op
  let (>=)      = let op = infixB 15 ">=" in call2 op
  let (<)       = let op = infixB 15 "<" in call2 op
  let (>)       = let op = infixB 15 ">" in call2 op
  let (=~)      = let op = infixB 15 "~" in fun x re -> call2 op x (string re)
  let ( =~* )   = let op = infixB 15 "~*" in fun x re -> call2 op x (string re)
  let (=~@)     = let op = infixB 15 "@@" in fun x p -> call2 op x (string p)
  let like      = let op = infixB 15 "like" in fun x p -> call2 op x (string p)
  let ilike     = let op = infixB 15 "ilike" in fun x p -> call2 op x (string p)

  let (~-)      = let op = prefix 25 "-" in call1 op
  let (+)       = let op = infixL 20 "+" in call2 op
  let (-)       = let op = infixL 20 "-" in call2 op
  let ( * )     = let op = infixL 21 "*" in call2 op
  let (/)       = let op = infixL 21 "/" in call2 op
  let (mod)     = let op = infixL 21 "%" in call2 op
  let (~-.)     = let op = prefix 25 "-" in call1 op
  let (+.)      = let op = infixL 20 "+" in call2 op
  let (-.)      = let op = infixL 20 "-" in call2 op
  let ( *. )    = let op = infixL 21 "*" in call2 op
  let (/.)      = let op = infixL 21 "/" in call2 op

  let date_part = let f = Func "date_part" in fun p x -> call2 f (string p) x
  let at_tz     = let op = infixB 18 " at time zone " in
                  fun tz x -> call2 op (string tz) x
end
