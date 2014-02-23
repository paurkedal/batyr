(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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

open Batyr_prereq
open CalendarLib
open Postgresql
open Printf
open Unprime_array
open Unprime_list
open Unprime_string

let section = Lwt_log.Section.make "batyr.db"

exception Response_error of string
let raise_resperr fmt = ksprintf (fun s -> raise (Response_error s)) fmt
let fail_resperr fmt = ksprintf (fun s -> Lwt.fail (Response_error s)) fmt

let or_null = function None -> Postgresql.null | Some s -> s

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

module Decode = struct
  type 'a t = string array -> int -> 'a * int

  let ( ** ) f g row i =
    let x, i = f row i in
    let y, i = g row i in
    (x, y), i

  let unit row i =
    match row.(i) with
    | "" -> (), i + 1
    | _ -> raise_resperr "Non-empty response %s for unit." row.(i)

  let string row i = row.(i), i + 1

  let bool row i =
    match row.(i) with
    | "t" | "true" -> true, i + 1
    | "f" | "false" -> false, i + 1
    | _ -> raise_resperr "Cannot convert %s to bool." row.(i)

  let int row i =
    try int_of_string row.(i), i + 1 with
    | Failure _ -> raise_resperr "Cannot convert %s to int." row.(i)

  let float row i =
    try float_of_string row.(i), i + 1 with
    | Failure _ -> raise_resperr "Cannot convert %s to float." row.(i)

  let epoch row i =
    try epoch_of_timestamp row.(i), i + 1 with
    | Failure _ -> raise_resperr "Cannot interpret time %s." row.(i)

  let option decode row i =
    (if row.(i) = Postgresql.null then None else Some (fst (decode row i))),
    i + 1

  let call d row =
    try
      let x, i = d row 0 in
      if i < Array.length row then
	raise (Response_error "The returned row is too long.");
      x
    with Invalid_argument "index out of bounds" ->
      raise (Response_error "The returned row is too short.")
end

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
    mutable acc_params : string list;
    acc_buffer : Buffer.t;
  }

  let add_param acc s =
    bprintf acc.acc_buffer "$%d" acc.acc_index;
    acc.acc_index <- acc.acc_index + 1;
    acc.acc_params <- s :: acc.acc_params

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
      acc_params = [];
      acc_buffer = Buffer.create 512;
    } in
    to_sql' acc 0 e;
    (Buffer.contents acc.acc_buffer, Array.of_list (List.rev acc.acc_params))

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

  let not	= let op = prefix 12 "not" in call1 op
  let (&&)	= let op = infixA 11 "and" in call2 op
  let (||)	= let op = infixA 10 "or" in call2 op
  let is_null	= let op = suffix 18 "is null" in call1 op
  let is_not_null=let op = suffix 18 "is not null" in call1 op
  let (=)	= let op = infixB 15 "=" in call2 op
  let (<>)	= let op = infixB 15 "!=" in call2 op
  let (<=)	= let op = infixB 15 "<=" in call2 op
  let (>=)	= let op = infixB 15 ">=" in call2 op
  let (<)	= let op = infixB 15 "<" in call2 op
  let (>)	= let op = infixB 15 ">" in call2 op
  let (=~)	= let op = infixB 15 "~" in fun x re -> call2 op x (string re)
  let ( =~* )	= let op = infixB 15 "~*" in fun x re -> call2 op x (string re)
  let like	= let op = infixB 15 "like" in fun x p -> call2 op x (string p)
  let ilike	= let op = infixB 15 "ilike" in fun x p -> call2 op x (string p)

  let (~-)	= let op = prefix 25 "-" in call1 op
  let (+)	= let op = infixL 20 "+" in call2 op
  let (-)	= let op = infixL 20 "-" in call2 op
  let ( * )	= let op = infixL 21 "*" in call2 op
  let (/)	= let op = infixL 21 "/" in call2 op
  let (mod)	= let op = infixL 21 "%" in call2 op
  let (~-.)	= let op = prefix 25 "-" in call1 op
  let (+.)	= let op = infixL 20 "+" in call2 op
  let (-.)	= let op = infixL 20 "-" in call2 op
  let ( *. )	= let op = infixL 21 "*" in call2 op
  let (/.)	= let op = infixL 21 "/" in call2 op

  let date_part	= let f = Func "date_part" in fun p x -> call2 f (string p) x
  let at_tz	= let op = infixB 18 " at time zone " in
		  fun tz x -> call2 op (string tz) x
end

let check_command_ok r =
  match r#status with
  | Command_ok -> Lwt.return_unit
  | Bad_response | Nonfatal_error | Fatal_error -> failwith r#error
  | _ -> Lwt.fail (Response_error "Expected Command_ok or an error response.")

let check_tuples_ok r =
  match r#status with
  | Tuples_ok -> Lwt.return_unit
  | Bad_response | Nonfatal_error | Fatal_error -> failwith r#error
  | _ -> Lwt.fail (Response_error "Expected Tuples_ok or an error response.")

class connection
  ?host ?hostaddr ?port ?dbname ?user ?password ?options ?tty
  ?requiressl ?conninfo () =
object (self)
  inherit Postgresql.connection ?host ?hostaddr ?port ?dbname ?user ?password
				?options ?tty ?requiressl ?conninfo ()

  val mutable accounting = false
  val mutable n_queries = 0
  val mutable n_affected = 0
  val mutable n_transferred = 0

  method private cost =
      10000 * n_queries
    +   100 * n_affected
    +  1000 * n_transferred
  method start_accounting =
    n_queries <- 0;
    n_affected <- 0;
    n_transferred <- 0;
    accounting <- true
  method stop_accounting =
    accounting <- false;
    self#cost

  method private wait_for_result =
    let socket_fd = Lwt_unix.of_unix_file_descr (Obj.magic self#socket) in
    let rec hold () =
      self#consume_input;
      if self#is_busy then Lwt_unix.wait_read socket_fd >> hold ()
		      else Lwt.return_unit in
    hold ()

  method fetch_result =
    Lwt_log.debug_f ~section "Waiting for result." >>
    self#wait_for_result >>
    match_lwt Lwt.wrap (fun () -> self#get_result) with
    | None -> Lwt.return_none
    | Some r as r_opt ->
      if accounting then begin
	n_queries <- n_queries + 1;
	n_affected <- n_affected
		    + (match r#cmd_tuples with "" -> 0 | s -> int_of_string s);
	n_transferred <- n_transferred + r#ntuples
      end;
      Lwt.return r_opt

  method fetch_last_result =
    match_lwt self#fetch_result with
    | None -> fail_resperr "No response received from DB."
    | Some r ->
      Lwt_log.debug_f ~section "Received %d tuples.\n" r#ntuples >>
      begin match_lwt self#fetch_result with
      | None ->
	Lwt.return r
      | Some r ->
	fail_resperr "Multiple results recived from DB, expected one."
      end

  method query ?params ?binary_params qs =
    (if Lwt_log.Section.level section = Lwt_log.Debug then
      match params with
      | None -> Lwt_log.debug_f ~section "Sending query \"%s\"." qs
      | Some params ->
	let params = Array.to_list params in
	Lwt_log.debug_f ~section "Sending query \"%s\" [|%s|]." qs
			  (String.concat "; " (List.map String.escaped params))
     else Lwt.return_unit) >>
    Lwt.wrap (fun () -> self#send_query ?params ?binary_params qs) >>
    self#fetch_last_result

  method command ?params ?binary_params qs =
    self#query ?params ?binary_params qs >>= check_command_ok

  method query_single : 'a.
	    'a Decode.t ->
	    ?params: string array -> ?binary_params: bool array -> string ->
	    'a Lwt.t =
	fun decode ?params ?binary_params qs ->
    self#query ?params ?binary_params qs >>= fun r ->
    check_tuples_ok r >>= fun () ->
    match r#get_all with
    | [| tup |] -> Lwt.wrap (fun () -> Decode.call decode tup)
    | _ -> Lwt.fail (Response_error "Expected a single row result.")

  method query_option : 'a.
	    'a Decode.t ->
	    ?params: string array -> ?binary_params: bool array -> string ->
	    'a option Lwt.t =
	fun decode ?params ?binary_params qs ->
    self#query ?params ?binary_params qs >>= fun r ->
    check_tuples_ok r >>= fun () ->
    match r#get_all with
    | [| |] -> Lwt.return_none
    | [| tup |] -> Lwt.wrap (fun () -> Some (Decode.call decode tup))
    | _ -> Lwt.fail (Response_error "Expected at most one row in the result.")

  method query_array : 'a.
	    'a Decode.t ->
	    ?params: string array -> ?binary_params: bool array -> string ->
	    'a array Lwt.t =
	fun decode ?params ?binary_params qs ->
    self#query ?params ?binary_params qs >>= fun r ->
    check_tuples_ok r >>
    Lwt.wrap (fun () -> Array.map (Decode.call decode) r#get_all)

  method query_list : 'a.
	    'a Decode.t ->
	    ?params: string array -> ?binary_params: bool array -> string ->
	    'a list Lwt.t =
	fun decode ?params ?binary_params qs ->
    self#query ?params ?binary_params qs >>= fun r ->
    check_tuples_ok r >>
    Lwt.wrap (fun () ->
      List.sample (fun i -> Decode.call decode (r#get_tuple i)) r#ntuples)

end

let connect () =
  let open Batyr_config in
  let host = db_host_cp#get in
  let hostaddr = db_host_cp#get in
  let port = db_port_cp#get in
  let dbname = db_database_cp#get in
  let user = db_user_cp#get in
  let password = db_password_cp#get in
  Lwt.return (new connection ?host ?hostaddr ?port ?dbname ?user ?password ())

let pool = Lwt_pool.create 5 connect
let quick_pool = Lwt_pool.create 3 connect
let use ?(quick = false) = Lwt_pool.use (if quick then quick_pool else pool)
let use_accounted ?quick f =
  use ?quick (fun dbh -> dbh#start_accounting;
			 f dbh >|= fun qr -> dbh#stop_accounting, qr)
