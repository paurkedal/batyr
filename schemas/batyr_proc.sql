-- Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

CREATE OR REPLACE VIEW batyr.resource_jids AS
  SELECT resource_id,
       node_name || CASE node_name WHEN '' THEN '' ELSE '@' END
    || domain_name || CASE resource_name WHEN '' THEN '' ELSE '/' END
    || resource_name AS jid
  FROM batyr.resources NATURAL JOIN batyr.nodes NATURAL JOIN batyr.domains;

CREATE OR REPLACE
  FUNCTION batyr.make_domain(dom text) RETURNS integer
  LANGUAGE plpgsql AS
$$
DECLARE r integer;
BEGIN
  FOR r IN SELECT domain_id FROM batyr.domains WHERE domain_name = dom
  LOOP
    RETURN r;
  END LOOP;
  FOR r IN
    INSERT INTO batyr.domains (domain_name) VALUES (dom) RETURNING domain_id
  LOOP
    RETURN r;
  END LOOP;
END
$$;

CREATE OR REPLACE
  FUNCTION batyr.make_node(dn text, nn text) RETURNS integer
  LANGUAGE plpgsql AS
$$
DECLARE di integer; r integer;
BEGIN
  di := batyr.make_domain(dn);
  FOR r IN
    SELECT node_id FROM batyr.nodes WHERE domain_id = di AND node_name = nn
  LOOP
    RETURN r;
  END LOOP;
  FOR r IN
    INSERT INTO batyr.nodes (domain_id, node_name) VALUES (di, nn)
    RETURNING node_id
  LOOP
    RETURN r;
  END LOOP;
END
$$;

CREATE OR REPLACE
  FUNCTION batyr.make_resource(dn text, nn text, rn text) RETURNS integer
  LANGUAGE plpgsql AS
$$
DECLARE ni integer; r integer;
BEGIN
  ni := batyr.make_node(dn, nn);
  FOR r IN
    SELECT resource_id FROM batyr.resources
    WHERE node_id = ni AND resource_name = rn
  LOOP
      RETURN r;
  END LOOP;
  FOR r IN
    INSERT INTO batyr.resources (node_id, resource_name)
    VALUES (ni, rn) RETURNING resource_id
  LOOP
    RETURN r;
  END LOOP;
END
$$;

CREATE OR REPLACE
  FUNCTION batyr.update_muc_room(ni integer, a text, d text, t boolean)
    RETURNS boolean
  LANGUAGE plpgsql AS
$$
DECLARE r integer;
BEGIN
  FOR r IN SELECT true FROM batyr.muc_rooms WHERE node_id = ni
  LOOP
    UPDATE batyr.muc_rooms
       SET room_alias = a, room_description = d, transcribe = t
     WHERE node_id = ni;
    RETURN false;
  END LOOP;
  INSERT INTO
      batyr.muc_rooms (node_id, room_alias, room_description, transcribe)
    VALUES (ni, a, d, t);
  RETURN true;
END
$$;

CREATE OR REPLACE
  FUNCTION batyr.intenc_date(t timestamp with time zone) RETURNS integer
  LANGUAGE plpgsql AS
$$
BEGIN
  RETURN 256 * (256 * date_part('year', t) + date_part('month', t))
             + date_part('day', t);
END
$$;
