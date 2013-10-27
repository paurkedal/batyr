-- Rename the peers table to resources and correlated names.
ALTER TABLE batyr.peers RENAME TO resources;
ALTER INDEX batyr.peers_pkey RENAME TO resources_pkey;
ALTER INDEX batyr.peers_node_id_resource_key RENAME TO resources_node_id_resource_key;
BEGIN;
  ALTER TABLE batyr.resources
    ADD CONSTRAINT resources_node_id_fkey
    FOREIGN KEY (node_id) REFERENCES batyr.nodes(node_id);
  ALTER TABLE batyr.resources DROP CONSTRAINT peers_node_id_fkey;
END;
ALTER SEQUENCE batyr.peers_peer_id_seq RENAME TO resources_resource_id_seq;

-- Rename peer_id columns to resource_id.
ALTER TABLE batyr.resources RENAME COLUMN peer_id TO resource_id;
ALTER TABLE batyr.accounts RENAME COLUMN peer_id TO resource_id;
BEGIN;
  ALTER TABLE batyr.accounts
    ADD CONSTRAINT accounts_resource_id_fkey
    FOREIGN KEY (resource_id) REFERENCES batyr.resources(resource_id);
  ALTER TABLE batyr.accounts DROP CONSTRAINT accounts_peer_id_fkey;
END;
ALTER TABLE batyr.muc_presence RENAME COLUMN peer_id TO resource_id;
BEGIN;
  ALTER TABLE batyr.muc_presence
    ADD CONSTRAINT muc_presence_resource_id_fkey
    FOREIGN KEY (resource_id) REFERENCES batyr.resources(resource_id);
  ALTER TABLE batyr.muc_presence DROP CONSTRAINT muc_presence_peer_id_fkey;
END;

-- Rename resource column to resource_name.
ALTER TABLE batyr.resources RENAME COLUMN resource TO resource_name;

-- Drop affected views and functions.  This must be re-created by runnig
-- batyr_prec.sql.
DROP VIEW batyr.peer_jids;
DROP FUNCTION IF EXISTS batyr.make_peer(text, text, text);

-- Drop the auxid column from messages.
ALTER TABLE batyr.messages DROP COLUMN auxid;

-- Add author_id column to batyr.messages.
ALTER TABLE batyr.messages
  ADD COLUMN author_id integer REFERENCES batyr.resources;
