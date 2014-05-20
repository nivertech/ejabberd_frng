--
<<<<<<< HEAD:src/odbc/mysql.sql
-- ejabberd, Copyright (C) 2002-2012   ProcessOne
=======
-- ejabberd, Copyright (C) 2002-2014   ProcessOne
>>>>>>> upstream/master:sql/mysql.sql
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

<<<<<<< HEAD:src/odbc/mysql.sql
--   WARNING !!!
-- ejabberd creates the tables automatically.
-- This file is obsolete.
-- Read the ejabberd modules source code for up-to-date table schema.

-- Needs MySQL (at least 4.0.x) with innodb back-end
SET table_type=InnoDB;

--
-- Tables schemas keep from previous ejabberd versions
--

CREATE TABLE hosts (
    clusterid integer NOT NULL,
    host varchar(250) NOT NULL PRIMARY KEY,
    config text NOT NULL
) CHARACTER SET utf8;
=======
CREATE TABLE users (
    username varchar(250) PRIMARY KEY,
    password text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;


CREATE TABLE last (
    username varchar(250) PRIMARY KEY,
    seconds text NOT NULL,
    state text NOT NULl
) ENGINE=InnoDB CHARACTER SET utf8;


CREATE TABLE rosterusers (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text NOT NULL,
    type text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers(username(75), jid(75));
CREATE INDEX i_rosteru_username ON rosterusers(username);
CREATE INDEX i_rosteru_jid ON rosterusers(jid);

CREATE TABLE rostergroups (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    grp text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE INDEX pk_rosterg_user_jid ON rostergroups(username(75), jid(75));

CREATE TABLE sr_group (
    name varchar(250) NOT NULL,
    opts text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE TABLE sr_user (
    jid varchar(250) NOT NULL,
    grp varchar(250) NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE UNIQUE INDEX i_sr_user_jid_group ON sr_user(jid(75), grp(75));
CREATE INDEX i_sr_user_jid ON sr_user(jid);
CREATE INDEX i_sr_user_grp ON sr_user(grp);

CREATE TABLE spool (
    username varchar(250) NOT NULL,
    xml text NOT NULL,
    seq BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE INDEX i_despool USING BTREE ON spool(username);

CREATE TABLE vcard (
    username varchar(250) PRIMARY KEY,
    vcard mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE TABLE vcard_xupdate (
    username varchar(250) PRIMARY KEY,
    hash text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE TABLE vcard_search (
    username varchar(250) NOT NULL,
    lusername varchar(250) PRIMARY KEY,
    fn text NOT NULL,
    lfn varchar(250) NOT NULL,
    family text NOT NULL,
    lfamily varchar(250) NOT NULL,
    given text NOT NULL,
    lgiven varchar(250) NOT NULL,
    middle text NOT NULL,
    lmiddle varchar(250) NOT NULL,
    nickname text NOT NULL,
    lnickname varchar(250) NOT NULL,
    bday text NOT NULL,
    lbday varchar(250) NOT NULL,
    ctry text NOT NULL,
    lctry varchar(250) NOT NULL,
    locality text NOT NULL,
    llocality varchar(250) NOT NULL,
    email text NOT NULL,
    lemail varchar(250) NOT NULL,
    orgname text NOT NULL,
    lorgname varchar(250) NOT NULL,
    orgunit text NOT NULL,
    lorgunit varchar(250) NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8;

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
    username varchar(250) PRIMARY KEY,
    name varchar(250) NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE TABLE privacy_list (
    username varchar(250) NOT NULL,
    name varchar(250) NOT NULL,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE INDEX i_privacy_list_username  USING BTREE ON privacy_list(username);
CREATE UNIQUE INDEX i_privacy_list_username_name USING BTREE ON privacy_list (username(75), name(75));

CREATE TABLE privacy_list_data (
    id bigint,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE INDEX i_privacy_list_data_id ON privacy_list_data(id);

CREATE TABLE private_storage (
    username varchar(250) NOT NULL,
    namespace varchar(250) NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;
>>>>>>> upstream/master:sql/mysql.sql

INSERT INTO hosts (clusterid, host, config)
VALUES (1, 'localhost', '');

-- Not tested in mysql
CREATE TABLE roster_version (
    username varchar(250) PRIMARY KEY,
    version text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE TABLE pubsub_node (
  host text,
  node text,
  parent text,
  type text,
  nodeid bigint auto_increment primary key
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_parent ON pubsub_node(parent(120));
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node(host(20), node(120));

CREATE TABLE pubsub_node_option (
  nodeid bigint,
  name text,
  val text
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option(nodeid);
ALTER TABLE `pubsub_node_option` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_node_owner (
  nodeid bigint,
  owner text
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner(nodeid);
ALTER TABLE `pubsub_node_owner` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_state (
  nodeid bigint,
  jid text,
  affiliation character(1),
  subscriptions text,
  stateid bigint auto_increment primary key
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE INDEX i_pubsub_state_jid ON pubsub_state(jid(60));
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state(nodeid, jid(60));
ALTER TABLE `pubsub_state` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_item (
  nodeid bigint,
  itemid text,
  publisher text,
  creation text,
  modification text,
  payload text
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE INDEX i_pubsub_item_itemid ON pubsub_item(itemid(36));
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item(nodeid, itemid(36));
ALTER TABLE `pubsub_item` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_subscription_opt (
  subid text,
  opt_name varchar(32),
  opt_value text
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt(subid(32), opt_name(32));

<<<<<<< HEAD:src/odbc/mysql.sql
--
-- Tables schemas dumped from gen_storage
--

CREATE TABLE `last_activity` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `timestamp` bigint(20) DEFAULT NULL,
  `status` text,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_online_room` (
  `name` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `pid` text,
  PRIMARY KEY (`name`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_registered` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `nick` text,
  PRIMARY KEY (`user`(105),`host`(105)),
  KEY `muc_registered_nick` (`nick`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_room_affiliation` (
  `name` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `jid` varchar(255) DEFAULT NULL,
  `affiliation` varchar(255) DEFAULT NULL,
  `reason` varchar(255) DEFAULT NULL,
  KEY `muc_room_affiliation_bag` (`name`(75),`host`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_room_opt` (
  `name` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `opt` varchar(255) DEFAULT NULL,
  `val` varchar(255) DEFAULT NULL,
  KEY `muc_room_opt_bag` (`name`(75),`host`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `offline_msg` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `timestamp` bigint(20) DEFAULT NULL,
  `expire` bigint(20) DEFAULT NULL,
  `from` varchar(255) DEFAULT NULL,
  `to` varchar(255) DEFAULT NULL,
  `packet` text,
  KEY `offline_msg_bag` (`user`(75),`host`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `passwd` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `password` text,
  `storedkey` text,
  `serverkey` text,
  `salt` text,
  `iterationcount` int(11) DEFAULT NULL,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `privacy_default_list` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `name` text,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `privacy_list` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  KEY `privacy_list_bag` (`user`(75),`host`(75)),
  KEY `privacy_list_name` (`name`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `privacy_list_data` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  `type` varchar(255) DEFAULT NULL,
  `value` varchar(255) DEFAULT NULL,
  `action` varchar(255) DEFAULT NULL,
  `order` int(11) DEFAULT NULL,
  `match_all` varchar(255) DEFAULT NULL,
  `match_iq` varchar(255) DEFAULT NULL,
  `match_message` varchar(255) DEFAULT NULL,
  `match_presence_in` varchar(255) DEFAULT NULL,
  `match_presence_out` varchar(255) DEFAULT NULL,
  KEY `privacy_list_data_bag` (`user`(75),`host`(75)),
  KEY `privacy_list_data_name` (`name`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `private_storage` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `ns` varchar(255) NOT NULL DEFAULT '',
  `xml` text,
  PRIMARY KEY (`user`(105),`host`(105),`ns`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `rostergroup` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `jid` varchar(255) DEFAULT NULL,
  `grp` varchar(255) DEFAULT NULL,
  KEY `rostergroup_bag` (`user`(75),`host`(75),`jid`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `rosteritem` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `jid` varchar(255) NOT NULL DEFAULT '',
  `name` text,
  `subscription` text,
  `ask` text,
  `askmessage` text,
  PRIMARY KEY (`user`(105),`host`(105),`jid`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `vcard` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `vcard` text,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `vcard_search` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `username` text,
  `lusername` text,
  `fn` text,
  `lfn` text,
  `family` text,
  `lfamily` text,
  `given` text,
  `lgiven` text,
  `middle` text,
  `lmiddle` text,
  `nickname` text,
  `lnickname` text,
  `bday` text,
  `lbday` text,
  `ctry` text,
  `lctry` text,
  `locality` text,
  `llocality` text,
  `email` text,
  `lemail` text,
  `orgname` text,
  `lorgname` text,
  `orgunit` text,
  `lorgunit` text,
  PRIMARY KEY (`user`(105),`host`(105)),
  KEY `vcard_search_lusername` (`lusername`(75)),
  KEY `vcard_search_lfn` (`lfn`(75)),
  KEY `vcard_search_lfamily` (`lfamily`(75)),
  KEY `vcard_search_lgiven` (`lgiven`(75)),
  KEY `vcard_search_lmiddle` (`lmiddle`(75)),
  KEY `vcard_search_lnickname` (`lnickname`(75)),
  KEY `vcard_search_lbday` (`lbday`(75)),
  KEY `vcard_search_lctry` (`lctry`(75)),
  KEY `vcard_search_llocality` (`llocality`(75)),
  KEY `vcard_search_lemail` (`lemail`(75)),
  KEY `vcard_search_lorgname` (`lorgname`(75)),
  KEY `vcard_search_lorgunit` (`lorgunit`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
=======
CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE UNIQUE INDEX i_muc_room_name_host USING BTREE ON muc_room(name(75), host(75));

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE INDEX i_muc_registered_nick USING BTREE ON muc_registered(nick(75));
CREATE UNIQUE INDEX i_muc_registered_jid_host USING BTREE ON muc_registered(jid(75), host(75));

CREATE TABLE irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE UNIQUE INDEX i_irc_custom_jid_host USING BTREE ON irc_custom(jid(75), host(75));

CREATE TABLE motd (
    username varchar(250) PRIMARY KEY,
    xml text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE TABLE caps_features (
    node varchar(250) NOT NULL,
    subnode varchar(250) NOT NULL,
    feature text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

CREATE INDEX i_caps_features_node_subnode ON caps_features(node(75), subnode(75));
>>>>>>> upstream/master:sql/mysql.sql
