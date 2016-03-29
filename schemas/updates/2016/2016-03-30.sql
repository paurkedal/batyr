-- Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

DROP INDEX IF EXISTS batyr.messages_fts;

CREATE INDEX resources_resource_name_ts ON batyr.resources
  USING gin(to_tsvector('english', resource_name));
CREATE INDEX messages_body_ts ON batyr.messages
  USING gin(to_tsvector('english', body));
CREATE INDEX messages_subject_ts ON batyr.messages
  USING gin(to_tsvector('english', subject));
