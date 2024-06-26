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

module Decode = Decoders_yojson.Basic.Decode
module Encode = Decoders_yojson.Basic.Encode

type 'a decoder = 'a Decode.decoder
type 'a encoder = 'a Encode.encoder

let timestamp_ms_decoder =
  let open Decode in
  let* t_ms = one_of ["float", float; "int", int >|= float_of_int] in
  (match Ptime.of_float_s (t_ms /. 1000.0) with
   | Some ts -> succeed ts
   | None -> fail "The argument of $date is out of range.")

let rfc3339_date_decoder =
  let open Decode in
  let* s = string in
  (match Ptime.of_rfc3339 s with
   | Ok (t, _tz, _) -> succeed t
   | Error _ -> fail "Bad timestamp string.")

let ptime_decoder =
  let open Decode in
  one_of [
    "timestamp/ms", field "$date" timestamp_ms_decoder;
    "RFC-3339 date", rfc3339_date_decoder;
  ]

let ptime_encoder ts =
  let open Encode in
  obj ["$date", float (Ptime.to_float_s ts *. 1000.0)]

let ptime_option_decoder =
  let open Decode in
  one_of [
    "timestamp/ms", field "$date" (nullable timestamp_ms_decoder);
    "RFC-3339 date", map Option.some rfc3339_date_decoder;
  ]

let ptime_option_encoder ts =
  let open Encode in
  let convert ts = Ptime.to_float_s ts *. 1000.0 in
  obj ["$date", nullable float (Option.map convert ts)]
