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

open Batyr_prereq
open Postgresql
open Printf
open Unprime_array
open Unprime_list

let section = Lwt_log.Section.make "Terra_db"

exception Response_error of string
let raise_resperr fmt = ksprintf (fun s -> raise (Response_error s)) fmt
let fail_resperr fmt = ksprintf (fun s -> Lwt.fail (Response_error s)) fmt

let or_null = function None -> Postgresql.null | Some s -> s

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

  method private wait_for_result =
    let socket_fd = Lwt_unix.of_unix_file_descr (Obj.magic self#socket) in
    let rec hold () =
      self#consume_input;
      if self#is_busy then Lwt_unix.wait_read socket_fd >> hold ()
		      else Lwt.return_unit in
    hold ()

  method fetch_result =
    Lwt_log.debug_f ~section "Waiting for result." >>
    self#wait_for_result >> Lwt.wrap (fun () -> self#get_result)

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
    Lwt_log.debug_f ~section "Sending query \"%s\"." qs >>
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

let pool = Lwt_pool.create 5 (fun () -> Lwt.return (new connection ()))
let quick_pool = Lwt_pool.create 3 (fun () -> Lwt.return (new connection ()))
let use ?(quick = false) = Lwt_pool.use (if quick then quick_pool else pool)
