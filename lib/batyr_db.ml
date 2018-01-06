(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open CalendarLib
open Lwt.Infix
open Printf
open Unprime_array
open Unprime_list
open Unprime_string

module type CONNECTION = Caqti_lwt.CONNECTION

let section = Lwt_log.Section.make "batyr.db"

exception Runtime_error of string
exception Response_error of string
let raise_resperr fmt = ksprintf (fun s -> raise (Response_error s)) fmt
let fail_resperr fmt = ksprintf (fun s -> Lwt.fail (Response_error s)) fmt

let escape_like s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if c = '\\' || c = '%' || c = '_' then Buffer.add_char buf '\\';
      Buffer.add_char buf c)
    s;
  Buffer.contents buf

let timestamp_of_epoch x =
  let open Unix in
  let tm = gmtime x in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d"
          (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
          tm.tm_hour tm.tm_min tm.tm_sec

let epoch_of_timestamp ts =
  let sec, subsec =
    try
      let i = String.index ts '.' in
      String.slice 0 i ts, float_of_string (String.slice_from i ts)
    with Not_found ->
      ts, 0.0 in
  let caltime = Printer.Calendar.from_fstring "%F %T%z" (sec ^ "+0000") in
  Calendar.to_unixfloat caltime +. subsec

type param = Param : 'a Caqti_type.t * 'a -> param

module Expr = struct

  type op =
    | Func of string
    | Prefix of string * int * int
    | Suffix of string * int * int
    | Infix of string * int * int list

  type u =
    | Literal of string
(*
    | Bool of bool
    | Int of int
    | Float of float
*)
    | String of string
    | Var of string
    | Call of op * u list

  type 'a t = u

  let p_comma = 1

  type acc = {
    mutable acc_index : int;
    mutable acc_params : param;
    acc_buffer : Buffer.t;
  }

  let add_param acc s =
    bprintf acc.acc_buffer "$%d" acc.acc_index;
    acc.acc_index <- acc.acc_index + 1;
    let Param (param_type, param) = acc.acc_params in
    acc.acc_params <- Param (Caqti_type.(tup2 param_type string), (param, s))

  let rec to_sql' acc prec = function
    | Literal s -> Buffer.add_string acc.acc_buffer s
(*
    | Bool x -> add_param acc (string_of_bool x)
    | Int x -> add_param acc (string_of_int x)
    | Float x -> add_param acc (string_of_float x)
*)
    | String x -> add_param acc x
    | Var v -> Buffer.add_string acc.acc_buffer v
    | Call (Func name, args) ->
      Buffer.add_string acc.acc_buffer name;
      Buffer.add_char acc.acc_buffer '(';
      let is_first = ref true in
      List.iter
        (fun arg ->
          if !is_first then is_first := false else
            Buffer.add_string acc.acc_buffer ", ";
          to_sql' acc p_comma arg)
        args;
      Buffer.add_char acc.acc_buffer ')'
    | Call (Prefix (name, p, p0), [arg]) ->
      if prec > p then Buffer.add_char acc.acc_buffer '(';
      Buffer.add_string acc.acc_buffer name;
      Buffer.add_char acc.acc_buffer ' ';
      to_sql' acc p0 arg;
      if prec > p then Buffer.add_char acc.acc_buffer ')'
    | Call (Suffix (name, p, p0), [arg]) ->
      if prec > p then Buffer.add_char acc.acc_buffer '(';
      to_sql' acc p0 arg;
      Buffer.add_string acc.acc_buffer name;
      Buffer.add_char acc.acc_buffer ' ';
      if prec > p then Buffer.add_char acc.acc_buffer ')'
    | Call ((Prefix _ | Suffix _), _) -> assert false
    | Call (Infix (name, p, ps), args) ->
      if prec > p then Buffer.add_char acc.acc_buffer '(';
      let is_first = ref true in
      List.iter2
        (fun p arg ->
          if !is_first then is_first := false else begin
            Buffer.add_char acc.acc_buffer ' ';
            Buffer.add_string acc.acc_buffer name;
            Buffer.add_char acc.acc_buffer ' '
          end;
          to_sql' acc p arg)
        ps args;
      if prec > p then Buffer.add_char acc.acc_buffer ')'

  let to_sql ?(first_index = 1) e =
    let acc = {
      acc_index = first_index;
      acc_params = Param (Caqti_type.unit, ());
      acc_buffer = Buffer.create 512;
    } in
    to_sql' acc 0 e;
    (Buffer.contents acc.acc_buffer, acc.acc_params)

  let prefix prec name = Prefix (name, prec, prec)
  let suffix prec name = Suffix (name, prec, prec)
  let infixA prec name = Infix (name, prec, [prec; prec])
  let infixB prec name = Infix (name, prec, [prec + 1; prec + 1])
  let infixL prec name = Infix (name, prec, [prec; prec + 1])
  let infixR prec name = Infix (name, prec, [prec + 1; prec])
  let call1 op e0 = Call (op, [e0])
  let call2 op e0 e1 = Call (op, [e0; e1])
  let func1 name e0 = Call (Func name, [e0])
  let func2 name e0 e1 = Call (Func name, [e0; e1])

  let of_sql x = Literal x
  let of_sql_f fmt = ksprintf (fun s -> Literal s) fmt
(*
  let bool x = Bool x
  let int x = Int x
  let float x = Float x
*)
  let bool x = Literal (string_of_bool x)
  let int x = Literal (string_of_int x)
  let float x = Literal (string_of_float x)
  let string x = String x
  let string_f fmt = ksprintf (fun s -> String s) fmt
  let epoch x = String (timestamp_of_epoch x)
  let calendar ?(tz = "+00") cal =
    let cts = CalendarLib.Printer.Calendar.sprint "%F %T" cal in
    String (if tz.[0] = '+' then cts ^ tz else cts ^ " " ^ tz)
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

let pool =
  let open Batyr_config in
  let uri = Uri.of_string db_uri_cp#get in
  (match Caqti_lwt.connect_pool uri with
   | Ok pool -> pool
   | Error err -> Caqti_error.pp Format.std_formatter err; exit 69)

let use ?(quick = false) f =
  Caqti_lwt.Pool.use ~priority:(if quick then 1.0 else 0.0) f pool

let use_exn ?quick f =
  let f conn = f conn >|= function (Ok _ | Error (#Caqti_error.t)) as r -> r in
  use ?quick f >>= Caqti_lwt.or_fail

let time_multiplier = 1e6

let use_accounted ?quick f =
  let tS = Unix.time () in
  use ?quick f >|= fun r ->
  let tE = Unix.time () in
  (match r with
   | Ok y -> Ok ((tE -. tS) *. time_multiplier, y)
   | Error _ as r -> r)

let use_accounted_exn ?quick f =
  let f conn = f conn >|= function (Ok _ | Error (#Caqti_error.t)) as r -> r in
  use_accounted ?quick f >>= Caqti_lwt.or_fail
