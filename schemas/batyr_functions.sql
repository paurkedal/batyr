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

CREATE OR REPLACE FUNCTION batyr.make_domain(dom text) RETURNS integer AS $$
DECLARE r integer;
BEGIN
    FOR r IN SELECT domain_id FROM batyr.domains WHERE domain_name = dom
    LOOP
	RETURN r;
    END LOOP;
    FOR r IN INSERT INTO batyr.domains (domain_name) VALUES (dom)
	     RETURNING domain_id
    LOOP
	RETURN r;
    END LOOP;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION batyr.make_node(dn text, nn text)
		   RETURNS integer AS $$
DECLARE di integer; r integer;
BEGIN
    di := batyr.make_domain(dn);
    FOR r IN SELECT node_id FROM batyr.nodes
	      WHERE domain_id = di AND node_name = nn
    LOOP
	RETURN r;
    END LOOP;
    FOR r IN INSERT INTO batyr.nodes (domain_id, node_name) VALUES (di, nn)
	     RETURNING node_id
    LOOP
	RETURN r;
    END LOOP;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION batyr.make_peer(dn text, nn text, rn text)
		   RETURNS integer AS $$
DECLARE ni integer; r integer;
BEGIN
    ni := batyr.make_node(dn, nn);
    FOR r IN SELECT peer_id FROM batyr.peers
	      WHERE node_id = ni AND resource = rn
    LOOP
	RETURN r;
    END LOOP;
    FOR r IN INSERT INTO batyr.peers (node_id, resource) VALUES (ni, rn)
	     RETURNING peer_id
    LOOP
	RETURN r;
    END LOOP;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
    batyr.update_muc_room(ni integer, a text, d text, t boolean)
RETURNS boolean AS $$
DECLARE r integer;
BEGIN
    FOR r IN SELECT true FROM batyr.muc_rooms WHERE node_id = ni
    LOOP
	UPDATE batyr.muc_rooms
	   SET room_alias = a, room_description = d, transcribe = t
	 WHERE node_id = ni;
	RETURN false;
    END LOOP;
    INSERT INTO batyr.muc_rooms (node_id, room_alias, room_description,
				 transcribe)
	VALUES (ni, a, d, t);
    RETURN true;
END
$$ LANGUAGE plpgsql;
