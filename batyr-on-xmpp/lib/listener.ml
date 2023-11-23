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

open Erm_xml
open Xmpp_inst
open Lwt.Infix
open Lwt.Syntax
open Printf
open Scanf
open Unprime
open Unprime_list
open Unprime_option

let section = Lwt_log.Section.make "batyr.presence"

exception Session_shutdown

let message_type_of_xmpp = function
 | Chat.Normal -> `Normal
 | Chat.Chat -> `Chat
 | Chat.Groupchat -> `Groupchat
 | Chat.Headline -> `Headline

type 'a connection_state =
  | Shutdown
  | Connected of 'a
  | Disconnected of unit Lwt_condition.t

module Config = Config

module Make (B : Data_sig.S) = struct
  open B

  let messages, emit_message = Lwt_react.E.create ()

  type muc_session = {
    ms_room : Muc_room.t;
    ms_users_by_nick : (string, Muc_user.t) Hashtbl.t;
    ms_emit_presence : ?step: React.step ->
                       Chat.presence_content Chat.stanza -> unit;
    ms_presences : Chat.presence_content Chat.stanza React.E.t;
    ms_emit_message : ?step: React.step -> Message.t -> unit;
    ms_messages : Message.t React.E.t;
    ms_is_present : bool React.S.t;
  }

  type t = {
    cs_account : Account.t;
    mutable cs_chat : Chat.chat connection_state;
    cs_rooms : (int, muc_session) Hashtbl.t;
    cs_emit_presence : ?step: React.step ->
                       Chat.presence_content Chat.stanza -> unit;
    cs_presences : Chat.presence_content Chat.stanza React.E.t;
    cs_emit_message : ?step: React.step -> Message.t -> unit;
    cs_messages : Message.t React.E.t;
  }

  let chat_sessions = Hashtbl.create 11

  let on_muc_message _cs ms msg =
    let muc_author =
      let nick = Resource.resource_name (Message.sender msg) in
      try Muc_user.resource (Hashtbl.find ms.ms_users_by_nick nick)
      with Not_found -> None in
    if Muc_room.transcribe ms.ms_room then
      Message.store ?muc_author msg >>= Caqti_lwt.or_fail
    else
      Lwt.return_unit

  let convert_delay = function
   | None -> Ptime_clock.now ()
   | Some Chat.{delay_stamp; delay_legacy; _} ->
      Lwt_log.ign_debug_f ~section "Got delay stamp %s." delay_stamp;
      let delay_stamp_modern =
        if not delay_legacy then delay_stamp ^ "Z" else
        sscanf delay_stamp "%04d%02d%02dT%s" (sprintf "%04d-%02d-%02dT%sZ")
      in
      (match Ptime.of_rfc3339 delay_stamp_modern with
       | Ok (t, tz, _) ->
          assert (tz = Some 0);
          Lwt_log.ign_debug_f ~section "Converted delay stamp to %s"
            (Ptime.to_rfc3339 t);
          t
       | Error _ ->
          Lwt_log.ign_error_f ~section "Received invalid delay stamp %s"
            delay_stamp;
          Ptime_clock.now ())

  let on_message cs _chat stanza =
    let open Xml in
    List.iter
      (function
        | Xmlelement ((None, tag), _attrs, _els) ->
          Lwt_log.ign_debug_f ~section "Unprocessed tag %s" tag
        | Xmlelement ((Some ns, tag), _attrs, _els) ->
          Lwt_log.ign_debug_f ~section "Unprocessed tag [%s]:%s" ns tag
        | Xmlcdata s ->
          Lwt_log.ign_debug_f ~section "Unprocessed cdata \"%s\""
                              (String.escaped s))
      Chat.(stanza.x);
    match Chat.(stanza.jid_from, stanza.jid_to, stanza.content.message_type) with
    | Some sender, Some recipient, Some message_type ->
      let seen_time = convert_delay Chat.(stanza.content.message_delay) in
      let sender = Resource.of_jid sender in
      let muc_room = Muc_room.cached_of_node (Resource.node sender) in
      let recipient = Resource.of_jid (JID.of_string recipient) in
      let subject = stanza.Chat.content.Chat.subject in
      let thread = stanza.Chat.content.Chat.thread in
      let body = stanza.Chat.content.Chat.body in
      if subject = None && thread = None && body = None then Lwt.return_unit else
      begin
        let msg =
          Message.make
            ~seen_time ~sender ~recipient
            ~message_type:(message_type_of_xmpp message_type)
            ?subject ?thread ?body () in
        let step = React.Step.create () in        (* NB! No yield until ... *)
        cs.cs_emit_message ~step msg;
        emit_message ~step msg;
        match muc_room with
        | None ->
          React.Step.execute step;                (* ... here and *)
          Lwt.return_unit
        | Some muc_room ->
          let room_node = Muc_room.node muc_room in
          let room_id = Option.get (Node.cached_id room_node) in
          (match Hashtbl.find cs.cs_rooms room_id with
           | exception Not_found ->
              React.Step.execute step;            (* ... here and *)
              Lwt_log.error_f "No MUC session established for %s."
                              (Node.to_string room_node)
           | ms ->
              ms.ms_emit_message ~step msg;
              React.Step.execute step;            (* ... here. *)
              on_muc_message cs ms msg)
      end
    | _ -> Lwt.return_unit

  let extract_muc_user nick =
    List.find_map
      begin function
      | Xml.Xmlelement ((_ns_muc_user, "x"), _, _) as el ->
        let user = Chat_muc.User.decode el in
        begin match user.Chat_muc.User.item with
        | None -> None
        | Some item ->
          let open Chat_muc in
          let jid = item.User.jid in
          let role = Option.get_or RoleNone item.Chat_muc.User.role in
          let affiliation = Option.get_or AffiliationNone item.User.affiliation in
          Some (Muc_user.make ~nick ?jid ~role ~affiliation ())
        end
      | _ -> None
      end

  let entered_room_by_node cs node =
    Option.find_map
      (fun id ->
        try Some (Hashtbl.find cs.cs_rooms id)
        with Not_found -> None)
      (Node.cached_id node)

  let on_presence cs chat stanza =
    match stanza.Chat.jid_from with
    | None ->
      Lwt_log.warning ~section "Ignoring presence stanza lacking \"from\"."
    | Some sender_jid ->
      Lwt_log.debug_f ~section "Received %s from %s."
        (match Chat.(stanza.content.presence_type) with
         | Some pt -> Chat.string_of_presence_type pt
         | None -> "presence")
        (JID.string_of_jid sender_jid) >>= fun () ->
      begin match Chat.(stanza.content.presence_type) with
      | Some Chat.Subscribe ->
        Chat.send_presence chat ~jid_to:sender_jid ~kind:Chat.Subscribed ()
      | _ -> Lwt.return_unit
      end >>= fun () ->
      let entered_room_node = Node.of_jid (JID.bare_jid sender_jid) in
      let step = React.Step.create () in          (* NB! No yield until ... *)
      cs.cs_emit_presence ~step stanza;
      begin match entered_room_by_node cs entered_room_node with
      | None ->
        React.Step.execute step;                  (* ... here, and *)
        Lwt.return_unit
      | Some ms ->
        ms.ms_emit_presence ~step stanza;
        React.Step.execute step;                  (* ... here. *)
        let nick = sender_jid.JID.lresource in
        let user_opt = extract_muc_user nick stanza.Chat.x in
        begin match Chat.(stanza.content.presence_type), user_opt with
        | None, Some user ->
          Hashtbl.replace ms.ms_users_by_nick nick user;
          Lwt_log.debug_f ~section "User %s is available in %s."
                          (Muc_user.to_string user)
                          (Muc_room.to_string ms.ms_room)
        | Some Chat.Unavailable, Some user ->
          Hashtbl.remove ms.ms_users_by_nick nick;
          Lwt_log.debug_f ~section "User %s is unavailable in %s."
                          (Muc_user.to_string user)
                          (Muc_room.to_string ms.ms_room)
        | _ -> Lwt.return_unit
        end
      end

  let on_error ~kind _cs ?id ?jid_from ?jid_to ?lang:_ error =
    let jid_from = Option.map JID.string_of_jid jid_from in
    let props = []
      |> Option.fold (fun s acc -> sprintf "recipient = %s" s :: acc) jid_to
      |> Option.fold (fun s acc -> sprintf "sender = %s" s :: acc) jid_from
      |> Option.fold (fun s acc -> sprintf "id = %s" s :: acc) id in
    Lwt_log.error_f ~section "%s error; %s; %s" kind
                    (String.concat ", " props) error.StanzaError.err_text

  let track_presence_of my_jid is_present stanza =
    match Chat.(stanza.jid_from, stanza.content.presence_type) with
    | Some sender_jid, Some Chat.Unavailable when sender_jid = my_jid ->
      begin match
        List.filter_map
          (fun e -> try Chat_muc.User.((Option.get ((decode e).item)).reason)
                    with Not_found -> None)
          Chat.(stanza.x)
      with
      | [] ->
        Lwt_log.notice_f ~section "I was logged out as %s for unknown reason."
                         (JID.string_of_jid my_jid) >>= fun () ->
        Lwt.return_false
      | reason :: _ ->
        Lwt_log.notice_f ~section "I was logged out as %s because: %s"
                         (JID.string_of_jid my_jid) reason >>= fun () ->
        Lwt.return_false
      end
    | Some sender_jid, None when sender_jid = my_jid ->
      Lwt_log.info_f ~section "I logged in as %s." (JID.string_of_jid my_jid)
        >>= fun () ->
      Lwt.return_true
    | _ ->
      Lwt.return is_present

  [@@@ocaml.warning "-27"]
  let drive_signal
        ?(what = "reconnect")
        ?(fuzz = 0.1) ?(dt_edge = 1.0)
        ?(dt_min = 5.0) ?(dt_sat = 3600.0) ?(dt_avg = 86400.0)
        f s =
    let cond = Lwt_condition.create () in
    let driver () =
      let backoff = Batyr_core.Backoff.create () in
      while%lwt true do
        if React.S.value s
        then Lwt_condition.wait cond >>= fun () -> Lwt_unix.sleep dt_edge
        else
          let dt = Batyr_core.Backoff.next backoff in
          f () >>= fun () ->
          Lwt_log.info_f ~section "Will wait %.3g s before next %s." dt what
            >>= fun () ->
          Lwt_unix.sleep dt
      done in
    Lwt.async driver;
    React.S.trace (fun v -> if not v then Lwt_condition.signal cond ()) s
  [@@@ocaml.warning "+27"]

  let muc_handler cs (resource_id, nick, since) =
    let chat =
      match cs.cs_chat with
      | Connected chat -> chat
      | _ -> assert false in
    let since = Option.map Ptime.to_float_s since in
    let nick = Option.get_or (Chat.get_myjid chat).JID.node nick in
    let%lwt resource = Resource.stored_of_id resource_id in
    let room_node = Resource.node resource in
    let room_jid = Resource.jid resource in
    match%lwt Muc_room.stored_of_node room_node with
    | None ->
      Lwt_log.error_f ~section "Presence in non-room %s."
                      (Node.to_string room_node)
    | Some room ->
      (* FIXME: Need <delay/> *)
      let since_r = ref (Option.map ((+.) 0.05) since) in
      let muc_login () =
        let%lwt seconds =
          begin match !since_r with
          | None ->
            Lwt_log.info_f ~section "Entering %s, no previous logs."
                           (Resource.to_string resource) >>= fun () ->
            Lwt.return_none
          | Some t_disconn ->
            let t = Unix.time () -. t_disconn in
            Lwt_log.info_f ~section "Entering %s, seconds = %f"
                           (Resource.to_string resource) t >>= fun () ->
            Lwt.return (Some (int_of_float t))
          end in
        Chat_muc.enter_room ?seconds ~nick chat room_jid in
      let%lwt room_id = Node.stored_id room_node >|= Option.get in
      let ms_presences, ms_emit_presence = React.E.create () in
      let ms_messages, ms_emit_message = React.E.create () in
      let my_jid = JID.replace_resource (Resource.jid resource) nick in
      let ms_is_present =
        Lwt_react.S.fold_s (track_presence_of my_jid) false ms_presences in
      let ms_is_present =
        drive_signal ~what:"MUC login" muc_login ms_is_present in
      let ms_is_present =
        React.S.trace (function false -> since_r := Some (Unix.time ()) | _ -> ())
                      ms_is_present in
      let ms = {
        ms_room = room;
        ms_users_by_nick = Hashtbl.create 17;
        ms_is_present;
        ms_presences; ms_emit_presence;
        ms_messages; ms_emit_message;
      } in
      Hashtbl.replace cs.cs_rooms room_id ms;
      Lwt.return_unit

  let chat_handler cs chat =
    Chat.register_stanza_handler chat (Chat.ns_client, "message")
      (Chat.parse_message ~callback:(on_message cs)
                          ~callback_error:(on_error ~kind:"message"));
    Chat.register_stanza_handler chat (Chat.ns_client, "presence")
      (Chat.parse_presence ~callback:(on_presence cs)
                           ~callback_error:(on_error ~kind:"presence"));
    Chat_version.register chat;
    Chat_ping.register chat;
    Chat_disco.register_info chat;
    let account_resource = Account.resource cs.cs_account in
    Chat.send_presence chat ~jid_from:(Resource.jid account_resource)
                       ~show:Chat.ShowDND ~status:"logging" () >>= fun () ->
    match cs.cs_chat with
    | Connected _chat -> assert false
    | Shutdown -> Lwt.return_unit
    | Disconnected chat_cond ->
      cs.cs_chat <- Connected chat;
      Lwt_condition.broadcast chat_cond ();
      let account_id = Resource.cached_id account_resource |> Option.get in
      Db.use_exn (Sql.Presence.room_presence account_id) >>=
      Lwt_list.iter_s (muc_handler cs)

  let account_key account =
    let resource = Account.resource account in
    let port = Account.port account in
    let JID.{lnode; ldomain; lresource; _} = Resource.jid resource in
    (ldomain, port, lnode, lresource)

  let run_once cs =
    let resource = Account.resource cs.cs_account in
    let port = Account.port cs.cs_account in
    let password = Account.password cs.cs_account in
    let JID.{lnode; ldomain; lresource; _} = Resource.jid resource in
    let params = Chat_params.make ~server:ldomain ~port ~username:lnode
                                  ~password ~resource:lresource () in
    let clear_cs () =
      if cs.cs_chat <> Shutdown then
        cs.cs_chat <- Disconnected (Lwt_condition.create ());
      Hashtbl.clear cs.cs_rooms in
    try%lwt Xmpp_inst.with_chat (chat_handler cs) params with
    | End_of_file ->
      clear_cs ();
      Lwt_log.error_f ~section "Lost connection."
    | xc ->
      clear_cs ();
      Lwt_log.error_f ~section "Caught %s." (Printexc.to_string xc)
        >>= fun () ->
      Lwt_log.debug ~section (Printexc.get_backtrace ())

  let create account =
    let key = account_key account in
    if Hashtbl.mem chat_sessions key then
      raise (Failure ("Already logged into this account."));
    let cs_presences, cs_emit_presence = React.E.create () in
    let cs_messages, cs_emit_message = React.E.create () in
    let cs = {
      cs_account = account;
      cs_chat = Disconnected (Lwt_condition.create ());
      cs_rooms = Hashtbl.create 23;
      cs_emit_presence; cs_presences;
      cs_emit_message;  cs_messages;
    } in
    Hashtbl.add chat_sessions key cs;
    cs

  let start account =
    let cs = create account in
    let backoff = Batyr_core.Backoff.create () in
    let rec connect_loop () =
      let t_start = Unix.time () in
      run_once cs >>= fun () ->
      let t_dur = Unix.time () -. t_start in
      if cs.cs_chat = Shutdown then
        Lwt_log.info_f ~section "Session shut down after %g s." t_dur
      else
        let t_sleep = Batyr_core.Backoff.next backoff in
        Lwt_log.info_f ~section "Session lasted %g s, will re-connect in %g s."
                       t_dur t_sleep >>= fun () ->
        Lwt_unix.sleep t_sleep >>= fun () ->
        connect_loop ()
    in
    Lwt.async connect_loop;
    cs

  let start_all () = Account.all_active () >|= List.iter (ignore % start)

  let find account =
    try Some (Hashtbl.find chat_sessions (account_key account))
    with Not_found -> None

  let is_active cs = match cs.cs_chat with Connected _ -> true | _ -> false

  let rec with_chat f cs =
    match cs.cs_chat with
    | Shutdown -> Lwt.fail Session_shutdown
    | Disconnected cond -> Lwt_condition.wait cond >>= fun () -> with_chat f cs
    | Connected chat -> f chat

  let shutdown cs =
    let key = account_key cs.cs_account in
    Hashtbl.remove chat_sessions key;
    match cs.cs_chat with
    | Shutdown -> Lwt.return_unit
    | Disconnected chat_cond ->
      cs.cs_chat <- Shutdown;
      Lwt_condition.broadcast chat_cond ();
      Lwt.return_unit
    | Connected chat ->
      cs.cs_chat <- Shutdown;
      Chat.close_stream chat

end

let launch (config : Config.t) =
  let module D = (val Data.connect config.storage_uri) in
  let module L = Make (D) in
  let* account =
    let resource = D.Resource.of_string config.resource in
    let {Config.port; password; _} = config in
    let* account' = D.Account.of_resource resource in
    (match account' with
     | None ->
        D.Account.create ~resource ~port ~password ~is_active:false ()
     | Some account ->
        let+ () = D.Account.update ~port ~password account in
        account)
  in
  let cs = L.create account in
  let+ () = L.run_once cs in
  `Lost_connection
