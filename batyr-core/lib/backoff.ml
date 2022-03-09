(* Copyright (C) 2018--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

type t = {
  dt_avg : float;
  c_sat : float;
  lc_sat : float;
  lc_min : float;
  fuzz : float;
  mutable t_norm : float;
  mutable c_cur : float;
}

let create ?(dt_min = 5.0) ?(dt_sat = 3600.0)
           ?(dt_avg = 86400.0) ?(fuzz = 0.1) () =
  let c_sat = dt_avg /. dt_sat in
  let lc_sat = log c_sat in
  let lc_min = log (dt_avg /. dt_min) in
  { dt_avg; c_sat; lc_sat; lc_min; fuzz;
    t_norm = 0.0; c_cur = 0.0; }

let next b =
  let t = Unix.time () in
  let dt = t -. b.t_norm in
  b.t_norm <- t;
  b.c_cur <- 1.0 +. b.c_cur *. exp (-. dt /. b.dt_avg);
  let p = b.c_cur /. b.c_sat in
  b.dt_avg *. exp ((p -. 1.0) *. b.lc_min -. p *. b.lc_sat)
           *. (1.0 +. b.fuzz *. (1.0 -. Random.float 2.0))
