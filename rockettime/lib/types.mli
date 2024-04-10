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

type 'a decoder = 'a Decoders_yojson.Basic.Decode.decoder
type 'a encoder = 'a Decoders_yojson.Basic.Encode.encoder

val ptime_decoder : Ptime.t decoder
val ptime_encoder : Ptime.t encoder
val ptime_option_decoder : Ptime.t option decoder
val ptime_option_encoder : Ptime.t option encoder
