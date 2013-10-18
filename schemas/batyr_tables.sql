-- Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

CREATE SCHEMA batyr;

CREATE TYPE batyr.operator_type AS ENUM ('person', 'bot', 'chatroom');

CREATE TABLE batyr.operators (
    operator_id SERIAL PRIMARY KEY,
    operator_name text UNIQUE NOT NULL,
    operator_type batyr.operator_type NOT NULL
);

CREATE TABLE batyr.peers (
    peer_id SERIAL PRIMARY KEY,
    jid text UNIQUE NOT NULL,
    operator_id integer REFERENCES batyr.operators,
    transcribe boolean NOT NULL
);

CREATE TABLE batyr.accounts (
    peer_id integer PRIMARY KEY REFERENCES batyr.peers,
    server_port integer NOT NULL DEFAULT 5222,
    client_password text NOT NULL,
    is_active boolean NOT NULL
);

CREATE TABLE batyr.chatrooms (
    peer_id integer PRIMARY KEY REFERENCES batyr.peers,
    account_id integer NOT NULL REFERENCES batyr.accounts,
    is_joined boolean NOT NULL
);

CREATE TYPE batyr.message_type
    AS ENUM ('normal', 'chat', 'groupchat', 'headline');

CREATE TABLE batyr.messages (
    message_id SERIAL PRIMARY KEY,
    auxid text,
    seen_time timestamp NOT NULL DEFAULT (current_timestamp AT TIME ZONE 'UTC'),
    sender_id integer NOT NULL REFERENCES batyr.peers,
    recipient_id integer REFERENCES batyr.peers,
    message_type batyr.message_type,
    subject text,
    thread text,
    body text
);
