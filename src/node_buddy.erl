%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%%
%%% @copyright 2006-2012 ProcessOne
=======
%%% Portions created by ProcessOne are Copyright 2006-2014, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2014, ProcessOne.
%%%
%%%
%%% @copyright 2006-2014 ProcessOne
>>>>>>> upstream/master:src/node_buddy.erl
%%% @author Christophe romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(node_buddy).

-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
=======

-include("jlib.hrl").
>>>>>>> upstream/master:src/node_buddy.erl

-behaviour(gen_pubsub_node).

%% Note on function definition
%%   included is all defined plugin function
%%   it's possible not to define some function at all
%%   in that case, warning will be generated at compilation
%%   and function call will fail,
%%   then mod_pubsub will call function from node_flat
%%   (this makes code cleaner, but execution a little bit longer)

%% API definition
<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
-export([init/3, terminate/2,
	 options/0, features/0,
	 create_node_permission/6,
	 create_node/2,
	 delete_node/1,
	 purge_node/2,
	 subscribe_node/8,
	 unsubscribe_node/4,
	 publish_item/6,
	 delete_item/4,
	 remove_extra_items/3,
	 get_entity_affiliations/2,
	 get_node_affiliations/1,
	 get_affiliation/2,
	 set_affiliation/3,
	 get_entity_subscriptions/2,
	 get_node_subscriptions/1,
	 get_subscriptions/2,
	 set_subscriptions/4,
	 get_pending_nodes/2,
	 get_states/1,
	 get_state/2,
	 set_state/1,
	 get_items/6,
	 get_items/2,
	 get_item/7,
	 get_item/2,
	 set_item/1,
	 get_item_name/3,
     node_to_path/1,
     path_to_node/1
	]).

=======
-export([init/3, terminate/2, options/0, features/0,
	 create_node_permission/6, create_node/2, delete_node/1,
	 purge_node/2, subscribe_node/8, unsubscribe_node/4,
	 publish_item/6, delete_item/4, remove_extra_items/3,
	 get_entity_affiliations/2, get_node_affiliations/1,
	 get_affiliation/2, set_affiliation/3,
	 get_entity_subscriptions/2, get_node_subscriptions/1,
	 get_subscriptions/2, set_subscriptions/4,
	 get_pending_nodes/2, get_states/1, get_state/2,
	 set_state/1, get_items/6, get_items/2, get_item/7,
	 get_item/2, set_item/1, get_item_name/3, node_to_path/1,
	 path_to_node/1]).
>>>>>>> upstream/master:src/node_buddy.erl

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost).

options() ->
<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
    [{deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, true},
     {notify_sub, false},
     {purge_offline, false},
     {persist_items, true},
     {max_items, ?MAXITEMS},
     {subscribe, true},
     {access_model, presence},
     {roster_groups_allowed, []},
=======
    [{deliver_payloads, true}, {notify_config, false},
     {notify_delete, false}, {notify_retract, true},
     {purge_offline, false}, {persist_items, true},
     {max_items, ?MAXITEMS}, {subscribe, true},
     {access_model, presence}, {roster_groups_allowed, []},
>>>>>>> upstream/master:src/node_buddy.erl
     {publish_model, publishers},
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, never},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

features() ->
<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
    ["create-nodes",
     "delete-nodes",
     "delete-items",
     "instant-nodes",
     "item-ids",
     "outcast-affiliation",
     "persistent-items",
     "publish",
     "purge-nodes",
     "retract-items",
     "retrieve-affiliations",
     "retrieve-items",
     "retrieve-subscriptions",
     "subscribe",
     "subscription-notifications"
    ].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_flat:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).
=======
    [<<"create-nodes">>, <<"delete-nodes">>,
     <<"delete-items">>, <<"instant-nodes">>, <<"item-ids">>,
     <<"outcast-affiliation">>, <<"persistent-items">>,
     <<"publish">>, <<"purge-nodes">>, <<"retract-items">>,
     <<"retrieve-affiliations">>, <<"retrieve-items">>,
     <<"retrieve-subscriptions">>, <<"subscribe">>,
     <<"subscription-notifications">>].

create_node_permission(Host, ServerHost, Node,
		       ParentNode, Owner, Access) ->
    node_hometree:create_node_permission(Host, ServerHost,
					 Node, ParentNode, Owner, Access).
>>>>>>> upstream/master:src/node_buddy.erl

create_node(NodeId, Owner) ->
    node_flat:create_node(NodeId, Owner).

delete_node(Removed) ->
    node_flat:delete_node(Removed).

<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
subscribe_node(NodeId, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat:subscribe_node(NodeId, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup, Options).

unsubscribe_node(NodeId, Sender, Subscriber, SubId) ->
    node_flat:unsubscribe_node(NodeId, Sender, Subscriber, SubId).

publish_item(NodeId, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_flat:publish_item(NodeId, Publisher, Model, MaxItems, ItemId, Payload).

remove_extra_items(NodeId, MaxItems, ItemIds) ->
    node_flat:remove_extra_items(NodeId, MaxItems, ItemIds).

delete_item(NodeId, Publisher, PublishModel, ItemId) ->
    node_flat:delete_item(NodeId, Publisher, PublishModel, ItemId).
=======
subscribe_node(NodeId, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_hometree:subscribe_node(NodeId, Sender, Subscriber,
				 AccessModel, SendLast, PresenceSubscription,
				 RosterGroup, Options).

unsubscribe_node(NodeId, Sender, Subscriber, SubID) ->
    node_hometree:unsubscribe_node(NodeId, Sender,
				   Subscriber, SubID).

publish_item(NodeId, Publisher, Model, MaxItems, ItemId,
	     Payload) ->
    node_hometree:publish_item(NodeId, Publisher, Model,
			       MaxItems, ItemId, Payload).

remove_extra_items(NodeId, MaxItems, ItemIds) ->
    node_hometree:remove_extra_items(NodeId, MaxItems,
				     ItemIds).

delete_item(NodeId, Publisher, PublishModel, ItemId) ->
    node_hometree:delete_item(NodeId, Publisher,
			      PublishModel, ItemId).
>>>>>>> upstream/master:src/node_buddy.erl

purge_node(NodeId, Owner) ->
    node_flat:purge_node(NodeId, Owner).

get_entity_affiliations(Host, Owner) ->
    node_flat:get_entity_affiliations(Host, Owner).

get_node_affiliations(NodeId) ->
    node_flat:get_node_affiliations(NodeId).

get_affiliation(NodeId, Owner) ->
    node_flat:get_affiliation(NodeId, Owner).

set_affiliation(NodeId, Owner, Affiliation) ->
<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
    node_flat:set_affiliation(NodeId, Owner, Affiliation).
=======
    node_hometree:set_affiliation(NodeId, Owner,
				  Affiliation).
>>>>>>> upstream/master:src/node_buddy.erl

get_entity_subscriptions(Host, Owner) ->
    node_flat:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(NodeId) ->
    node_flat:get_node_subscriptions(NodeId).

get_subscriptions(NodeId, Owner) ->
    node_flat:get_subscriptions(NodeId, Owner).

set_subscriptions(NodeId, Owner, Subscription, SubId) ->
<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
    node_flat:set_subscriptions(NodeId, Owner, Subscription, SubId).
=======
    node_hometree:set_subscriptions(NodeId, Owner,
				    Subscription, SubId).
>>>>>>> upstream/master:src/node_buddy.erl

get_pending_nodes(Host, Owner) ->
    node_flat:get_pending_nodes(Host, Owner).


<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
get_states(NodeId) ->
    node_flat:get_states(NodeId).
=======
get_states(NodeId) -> node_hometree:get_states(NodeId).
>>>>>>> upstream/master:src/node_buddy.erl

get_state(NodeId, JID) ->
    node_flat:get_state(NodeId, JID).

<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
set_state(State) ->
    node_flat:set_state(State).
=======
set_state(State) -> node_hometree:set_state(State).
>>>>>>> upstream/master:src/node_buddy.erl

get_items(NodeId, From) ->
    node_flat:get_items(NodeId, From).

<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat:get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).
=======
get_items(NodeId, JID, AccessModel,
	  PresenceSubscription, RosterGroup, SubId) ->
    node_hometree:get_items(NodeId, JID, AccessModel,
			    PresenceSubscription, RosterGroup, SubId).
>>>>>>> upstream/master:src/node_buddy.erl

get_item(NodeId, ItemId) ->
    node_flat:get_item(NodeId, ItemId).

<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
get_item(NodeId, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat:get_item(NodeId, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_flat:set_item(Item).
=======
get_item(NodeId, ItemId, JID, AccessModel,
	 PresenceSubscription, RosterGroup, SubId) ->
    node_hometree:get_item(NodeId, ItemId, JID, AccessModel,
			   PresenceSubscription, RosterGroup, SubId).

set_item(Item) -> node_hometree:set_item(Item).
>>>>>>> upstream/master:src/node_buddy.erl

get_item_name(Host, Node, Id) ->
    node_flat:get_item_name(Host, Node, Id).

<<<<<<< HEAD:src/mod_pubsub/node_buddy.erl
node_to_path(Node) ->
    node_flat:node_to_path(Node).

path_to_node(Path) ->
    node_flat:path_to_node(Path).
=======
node_to_path(Node) -> node_flat:node_to_path(Node).

path_to_node(Path) -> node_flat:path_to_node(Path).
>>>>>>> upstream/master:src/node_buddy.erl
