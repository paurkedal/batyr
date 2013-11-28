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

CREATE TABLE batyr.domains (
    domain_id SERIAL PRIMARY KEY,
    domain_name text UNIQUE NOT NULL
);

CREATE TABLE batyr.nodes (
    node_id SERIAL PRIMARY KEY,
    domain_id integer NOT NULL REFERENCES batyr.domains,
    node_name text NOT NULL,
    UNIQUE (domain_id, node_name)
);

CREATE TABLE batyr.resources (
    resource_id SERIAL PRIMARY KEY,
    node_id integer NOT NULL REFERENCES batyr.nodes,
    resource_name text NOT NULL,
    UNIQUE (node_id, resource_name)
);

CREATE TABLE batyr.accounts (
    resource_id integer PRIMARY KEY REFERENCES batyr.resources,
    server_port integer NOT NULL DEFAULT 5222,
    client_password text NOT NULL,
    is_active boolean NOT NULL
);

CREATE TABLE batyr.muc_rooms (
    node_id integer PRIMARY KEY REFERENCES batyr.nodes,
    room_alias text UNIQUE,
    room_description text,
    transcribe boolean NOT NULL
);

CREATE TABLE batyr.muc_presence (
    resource_id integer PRIMARY KEY REFERENCES batyr.resources,
    account_id integer NOT NULL REFERENCES batyr.accounts,
    is_present boolean NOT NULL,
    nick text
);

CREATE TYPE batyr.message_type
    AS ENUM ('normal', 'chat', 'groupchat', 'headline');

CREATE TABLE batyr.messages (
    message_id SERIAL PRIMARY KEY,
    seen_time timestamp NOT NULL DEFAULT (current_timestamp AT TIME ZONE 'UTC'),
    sender_id integer NOT NULL REFERENCES batyr.resources,
    author_id integer REFERENCES batyr.resources,
    recipient_id integer REFERENCES batyr.resources,
    message_type batyr.message_type,
    subject text,
    thread text,
    body text
);
