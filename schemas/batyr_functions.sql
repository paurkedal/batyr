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

CREATE OR REPLACE FUNCTION batyr.make_operator(t batyr.operator_type, n text)
		   RETURNS integer AS $$
DECLARE r integer;
BEGIN
    FOR r IN SELECT operator_id FROM batyr.operators
	      WHERE operator_type = t AND operator_name = n
    LOOP
	RETURN r;
    END LOOP;
    FOR r IN INSERT INTO batyr.operators (operator_type, operator_name)
		  VALUES (t, n) RETURNING operator_id
    LOOP
	RETURN r;
    END LOOP;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION batyr.make_jid(jid_ text, tscr boolean)
		   RETURNS integer AS $$
DECLARE r integer;
BEGIN
    FOR r IN SELECT peer_id FROM batyr.peers WHERE jid = jid_
    LOOP
	RETURN r;
    END LOOP;
    FOR r IN INSERT INTO batyr.peers (jid, transcribe)
		  VALUES (jid_, tscr) RETURNING peer_id
    LOOP
	RETURN r;
    END LOOP;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION batyr.connect(t batyr.operator_type, n text, j text)
		   RETURNS void AS $$
DECLARE oi integer; ji integer;
BEGIN
    oi := batyr.make_operator(t, n);
    ji := batyr.make_jid(j, false);
    UPDATE batyr.peers SET operator_id = oi WHERE peer_id = ji;
END
$$ LANGUAGE plpgsql;
