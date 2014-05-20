--
<<<<<<< HEAD:src/odbc/pg.sql
-- ejabberd, Copyright (C) 2002-2012   ProcessOne
=======
-- ejabberd, Copyright (C) 2002-2014   ProcessOne
>>>>>>> upstream/master:sql/pg.sql
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--                         
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--

--   WARNING !!!
-- ejabberd creates the tables automatically.
-- This file is obsolete.
-- Read the ejabberd modules source code for up-to-date table schema.

CREATE TABLE users (
<<<<<<< HEAD:src/odbc/pg.sql
    username text NOT NULL,
    host text NOT NULL,
    "password" text NOT NULL,
    PRIMARY KEY (host, username)
=======
    username text PRIMARY KEY,
    "password" text NOT NULL,
>>>>>>> upstream/master:sql/pg.sql
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE last (
    username text NOT NULL,
    host text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL,
    PRIMARY KEY (host, username)
);

CREATE TABLE rosterusers (
    username text NOT NULL,
    host text NOT NULL,
    jid text NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text,
    "type" text,
<<<<<<< HEAD:src/odbc/pg.sql
    PRIMARY KEY (host, username, jid)
=======
>>>>>>> upstream/master:sql/pg.sql
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE rostergroups (
    username text NOT NULL,
    host text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL,
    PRIMARY KEY (host, username, jid)
);

<<<<<<< HEAD:src/odbc/pg.sql
=======
CREATE INDEX pk_rosterg_user_jid ON rostergroups USING btree (username, jid);

CREATE TABLE sr_group (
    name text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE sr_user (
    jid text NOT NULL,
    grp text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_sr_user_jid_grp ON sr_user USING btree (jid, grp);
CREATE INDEX i_sr_user_jid ON sr_user USING btree (jid);
CREATE INDEX i_sr_user_grp ON sr_user USING btree (grp);

>>>>>>> upstream/master:sql/pg.sql
CREATE TABLE spool (
    username text NOT NULL,
    host text NOT NULL,
    xml text NOT NULL,
    seq SERIAL,
<<<<<<< HEAD:src/odbc/pg.sql
    PRIMARY KEY (host, username, seq)
=======
>>>>>>> upstream/master:sql/pg.sql
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE vcard (
<<<<<<< HEAD:src/odbc/pg.sql
    username text NOT NULL,
    host text NOT NULL,
    vcard text NOT NULL,
    PRIMARY KEY (host, username)
=======
    username text PRIMARY KEY,
    vcard text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE vcard_xupdate (
    username text PRIMARY KEY,
    hash text NOT NULL,
>>>>>>> upstream/master:sql/pg.sql
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE vcard_search (
    host text NOT NULL,
    username text NOT NULL,
    lusername text NOT NULL,
    fn text NOT NULL,
    lfn text NOT NULL,
    family text NOT NULL,
    lfamily text NOT NULL,
    given text NOT NULL,
    lgiven text NOT NULL,
    middle text NOT NULL,
    lmiddle text NOT NULL,
    nickname text NOT NULL,
    lnickname text NOT NULL,
    bday text NOT NULL,
    lbday text NOT NULL,
    ctry text NOT NULL,
    lctry text NOT NULL,
    locality text NOT NULL,
    llocality text NOT NULL,
    email text NOT NULL,
    lemail text NOT NULL,
    orgname text NOT NULL,
    lorgname text NOT NULL,
    orgunit text NOT NULL,
    lorgunit text NOT NULL,
    PRIMARY KEY (host, lusername)
);

CREATE INDEX i_vcard_search_lusername ON vcard_search(lusername);
CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn);
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily);
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven);
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle);
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname);
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday);
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry);
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality);
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail);
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname);
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit);

CREATE TABLE privacy_default_list (
    username text NOT NULL,
    host text NOT NULL,
    name text NOT NULL,
    PRIMARY KEY (host, username)
);

CREATE TABLE privacy_list (
    username text NOT NULL,
    host text NOT NULL,
    name text NOT NULL,
    id SERIAL UNIQUE,
<<<<<<< HEAD:src/odbc/pg.sql
    PRIMARY KEY (host, username, name)
=======
>>>>>>> upstream/master:sql/pg.sql
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE privacy_list_data (
    id bigint REFERENCES privacy_list(id) ON DELETE CASCADE,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE private_storage (
    username text NOT NULL,
    host text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
<<<<<<< HEAD:src/odbc/pg.sql
    PRIMARY KEY (host, username, namespace)
=======
>>>>>>> upstream/master:sql/pg.sql
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE hosts (
    clusterid integer NOT NULL,
    host text NOT NULL,
    config text NOT NULL,
    PRIMARY KEY (host)
);


CREATE TABLE roster_version (
    username text PRIMARY KEY,
    version text NOT NULL
);

-- To update from 0.9.8:
-- CREATE SEQUENCE spool_seq_seq;
-- ALTER TABLE spool ADD COLUMN seq integer;
-- ALTER TABLE spool ALTER COLUMN seq SET DEFAULT nextval('spool_seq_seq');
-- UPDATE spool SET seq = DEFAULT;
-- ALTER TABLE spool ALTER COLUMN seq SET NOT NULL;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

CREATE TABLE pubsub_node (
  host text,
  node text,
  parent text,
  "type" text,
  nodeid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_node_parent ON pubsub_node USING btree (parent);
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node USING btree (host, node);

CREATE TABLE pubsub_node_option (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  name text,
  val text
);
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option USING btree (nodeid);

CREATE TABLE pubsub_node_owner (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  owner text
);
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner USING btree (nodeid);

CREATE TABLE pubsub_state (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  jid text,
  affiliation character(1),
  subscriptions text,
  stateid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_state_jid ON pubsub_state USING btree (jid);
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state USING btree (nodeid, jid);

CREATE TABLE pubsub_item (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  itemid text, 
  publisher text,
  creation text,
  modification text,
  payload text
);
CREATE INDEX i_pubsub_item_itemid ON pubsub_item USING btree (itemid);
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item USING btree (nodeid, itemid);

CREATE TABLE pubsub_subscription_opt (
  subid text,
  opt_name varchar(32),
  opt_value text
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt USING btree (subid, opt_name);

CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room USING btree (name, host);

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_muc_registered_nick ON muc_registered USING btree (nick);
CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered USING btree (jid, host);

CREATE TABLE irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_irc_custom_jid_host ON irc_custom USING btree (jid, host);

CREATE TABLE motd (
    username text PRIMARY KEY,
    xml text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE caps_features (
    node text NOT NULL,
    subnode text NOT NULL,
    feature text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_caps_features_node_subnode ON caps_features USING btree (node, subnode);
