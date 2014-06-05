(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

open Eliom_content
open Unprime_list

module Html5_R = struct

  let retain a b = Gc.finalise (fun _ -> ignore a) b

  let signal_is_const s = React.S.equal s (React.S.const (React.S.value s))

  let append_child_signal' p csig =
    let c = ref p##appendChild(Html5.To_dom.of_node (React.S.value csig)) in
    let update e =
      let n = Html5.To_dom.of_node e in
      Dom.replaceChild p n !c; c := n in
    if signal_is_const csig
    then None
    else Some (React.S.trace update csig)

  let append_child_signals p csigs =
    let updaters = List.filter_map (append_child_signal' p) csigs in
    if updaters <> [] then retain updaters p

  let div ?a esigs =
    let p_div = Html5.D.div ?a [] in
    append_child_signals (Html5.To_dom.of_div p_div) esigs;
    p_div

  let span ?a esigs =
    let p_span = Html5.D.span ?a [] in
    append_child_signals (Html5.To_dom.of_span p_span) esigs;
    p_span

end
