%%%----------------------------------------------------------------------
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
<<<<<<< HEAD
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
=======
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
>>>>>>> upstream/master
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

%%% Database schema (version / storage / table)
%%%
%%% In this module, timestamp is the timestamp expressed in milliseconds.
%%% in jlib.erl, the timestamp is a tuple {{Y,M,D},{H,M,S}} or {{Y,M,D},{H,M,S},{Unit,Value}
%%% where Unit = milliseconds
%%% and Value = integer() in the proper unit
%%%
%%% 2.1.x / mnesia / offline_msg
%%%  us = {Username::string(), Host::string()}
%%%  timestamp = now()
%%%  expire = never | ???
%%%  from = jid_old()
%%%  to = jid_old()
%%%  packet = xmlelement()
%%%
%%% 2.1.x / odbc / spool
%%%  username = varchar250
%%%  xml = text
%%%  seq = bigint unsigned
%%%
%%% 3.0.0-prealpha / mnesia / offline_msg
%%%  us = {Username::string(), Host::string()}
%%%  timestamp = now()
%%%  expire = never | ???
%%%  from = jid()
%%%  to = jid()
%%%  packet = #xmlel
%%%
%%% 3.0.0-prealpha / odbc / spool
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / offline_msg
%%%  user_host = {Username::binary(), Host::binary()}
%%%  timestamp = integer()
%%%  expire = 0 | integer()
%%%  from = jid()
%%%  to = jid()
%%%  packet = binary()
%%%
%%% 3.0.0-alpha / odbc / offline_msg
%%%  user = varchar150
%%%  host = varchar150
%%%  timestamp =  bigint
%%%  expire = bigint
%%%  from = varchar150
%%%  to = varchar150
%%%  packet = varchar150

-module(mod_offline).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([count_offline_messages/2]).

-export([start/2,
	 loop/2,
	 stop/1,
	 store_packet/3,
	 resend_offline_messages/2,
	 pop_offline_messages/3,
	 get_sm_features/5,
	 remove_expired_messages/1,
	 remove_old_messages/2,
	 remove_user/2,
<<<<<<< HEAD
	 get_queue_length/2,
=======
         import/1,
         import/3,
         export/1,
	 get_queue_length/2,
         get_offline_els/2,
>>>>>>> upstream/master
	 webadmin_page/3,
	 webadmin_user/4,
	 webadmin_user_parse_query/5]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

%% The packet is stored serialized as a binary
-record(offline_msg, {user_host, timestamp, expire, from, to, packet}).
=======
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-record(offline_msg,
	{us = {<<"">>, <<"">>} :: {binary(), binary()},
         timestamp = now()     :: erlang:timestamp() | '_',
         expire = now()        :: erlang:timestamp() | never | '_',
         from = #jid{}         :: jid() | '_',
         to = #jid{}           :: jid() | '_',
         packet = #xmlel{}     :: xmlel() | '_'}).
>>>>>>> upstream/master

-define(PROCNAME, ejabberd_offline).

-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

% These are the namespace already declared by the stream opening. This is
% used at serialization time.
-define(DEFAULT_NS, ?NS_JABBER_CLIENT).
-define(PREFIXED_NS, [{?NS_XMPP, ?NS_XMPP_pfx}]).

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).

<<<<<<< HEAD
start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
%    Host = binary_to_list(HostB),
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, HostB, offline_msg,
			     [{disc_only_copies, [node()]},
			      {odbc_host, HostB},
			      {type, bag},
			      {attributes, record_info(fields, offline_msg)},
			      {types, [{user_host, {text, text}},
				       {timestamp, bigint},
				       {expire, bigint},
				       {from, jid},
				       {to, jid}]}]),
    update_table(HostB, Backend),
    ejabberd_hooks:add(offline_message_hook, HostB,
		       ?MODULE, store_packet, 50),
    ejabberd_hooks:add(resend_offline_messages_hook, HostB,
=======
start(Host, Opts) ->
    case gen_mod:db_type(Opts) of
      mnesia ->
	  mnesia:create_table(offline_msg,
			      [{disc_only_copies, [node()]}, {type, bag},
			       {attributes, record_info(fields, offline_msg)}]),
	  update_table();
      _ -> ok
    end,
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       store_packet, 50),
    ejabberd_hooks:add(resend_offline_messages_hook, Host,
>>>>>>> upstream/master
		       ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(disco_sm_features, HostB,
		       ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_local_features, HostB,
		       ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(webadmin_page_host, HostB,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, HostB,
		       ?MODULE, webadmin_user, 50),
    ejabberd_hooks:add(webadmin_user_parse_query, HostB,
                       ?MODULE, webadmin_user_parse_query, 50),
<<<<<<< HEAD
    AccessMaxOfflineMsgs = gen_mod:get_opt(access_max_user_messages, Opts, max_user_offline_messages),
    register(gen_mod:get_module_proc(HostB, ?PROCNAME),
	     spawn(?MODULE, loop, [AccessMaxOfflineMsgs])).

stanza_to_store(Stanza) ->
    exmpp_xml:document_to_binary(Stanza).
store_to_stanza(StoreBinary) -> 
    [Xml] = exmpp_xml:parse_document(StoreBinary),
    Xml.

loop(AccessMaxOfflineMsgs) ->
    receive
	#offline_msg{user_host=US} = Msg ->
	    Msgs = receive_all(US, [Msg]),
	    Len = length(Msgs),
	    %% TODO: is lower?
	    {User, Host} = US,
	    MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs,
						   User, Host),
	    F = fun() ->
			%% Only count messages if needed:
			Count =
			    if MaxOfflineMsgs =/= infinity ->
				    Len + get_queue_length(User, Host);
				   true -> 
					0
				end,
			if
			    Count > MaxOfflineMsgs ->
				discard_warn_sender(Msgs);
			    true ->
				if
				    Len >= ?OFFLINE_TABLE_LOCK_THRESHOLD ->
					gen_storage:write_lock_table(Host, offline_msg);
				    true ->
					ok
				end,
				lists:foreach(fun(M) ->
						      gen_storage:write(Host, M)
					      end, Msgs)
			end
		end,
	    gen_storage:transaction(Host, offline_msg, F),
	    loop(AccessMaxOfflineMsgs);
	_ ->
	    loop(AccessMaxOfflineMsgs)
=======
    AccessMaxOfflineMsgs = gen_mod:get_opt(access_max_user_messages, Opts, fun(A) -> A end, max_user_offline_messages),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, loop, [Host, AccessMaxOfflineMsgs])).

loop(Host, AccessMaxOfflineMsgs) ->
    receive
        #offline_msg{us = UserServer} = Msg ->
            DBType = gen_mod:db_type(Host, ?MODULE),
	    Msgs = receive_all(UserServer, [Msg], DBType),
	    Len = length(Msgs),
	    MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs,
						   UserServer, Host),
            store_offline_msg(Host, UserServer, Msgs, Len, MaxOfflineMsgs, DBType),
            loop(Host, AccessMaxOfflineMsgs);
        _ ->
	    loop(Host, AccessMaxOfflineMsgs)
    end.

store_offline_msg(_Host, US, Msgs, Len, MaxOfflineMsgs,
		  mnesia) ->
    F = fun () ->
		Count = if MaxOfflineMsgs =/= infinity ->
			       Len +
				 p1_mnesia:count_records(offline_msg,
							 #offline_msg{us = US,
								      _ = '_'});
			   true -> 0
			end,
		if Count > MaxOfflineMsgs -> discard_warn_sender(Msgs);
		   true ->
		       if Len >= (?OFFLINE_TABLE_LOCK_THRESHOLD) ->
			      mnesia:write_lock_table(offline_msg);
			  true -> ok
		       end,
		       lists:foreach(fun (M) -> mnesia:write(M) end, Msgs)
		end
	end,
    mnesia:transaction(F);
store_offline_msg(Host, {User, _Server}, Msgs, Len, MaxOfflineMsgs, odbc) ->
    Count = if MaxOfflineMsgs =/= infinity ->
		   Len + count_offline_messages(User, Host);
	       true -> 0
	    end,
    if Count > MaxOfflineMsgs -> discard_warn_sender(Msgs);
       true ->
	   Query = lists:map(fun (M) ->
				     Username =
					 ejabberd_odbc:escape((M#offline_msg.to)#jid.luser),
				     From = M#offline_msg.from,
				     To = M#offline_msg.to,
				     #xmlel{name = Name, attrs = Attrs,
					    children = Els} =
					 M#offline_msg.packet,
				     Attrs2 =
					 jlib:replace_from_to_attrs(jlib:jid_to_string(From),
								    jlib:jid_to_string(To),
								    Attrs),
				     Packet = #xmlel{name = Name,
						     attrs = Attrs2,
						     children =
							 Els ++
							   [jlib:timestamp_to_xml(calendar:now_to_universal_time(M#offline_msg.timestamp),
										  utc,
										  jlib:make_jid(<<"">>,
												Host,
												<<"">>),
										  <<"Offline Storage">>),
							    jlib:timestamp_to_xml(calendar:now_to_universal_time(M#offline_msg.timestamp))]},
				     XML =
					 ejabberd_odbc:escape(xml:element_to_binary(Packet)),
				     odbc_queries:add_spool_sql(Username, XML)
			     end,
			     Msgs),
	   odbc_queries:add_spool(Host, Query)
>>>>>>> upstream/master
    end.

%% Function copied from ejabberd_sm.erl:
get_max_user_messages(AccessRule, {User, Server}, Host) ->
    case acl:match_rule(
<<<<<<< HEAD
	   Host, AccessRule, exmpp_jid:make(LUser, Host, "")) of
=======
	   Host, AccessRule, jlib:make_jid(User, Server, <<"">>)) of
>>>>>>> upstream/master
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_MESSAGES
    end.

receive_all(US, Msgs, DBType) ->
    receive
<<<<<<< HEAD
	#offline_msg{user_host=US} = Msg ->
	    receive_all(US, [Msg | Msgs])
    after 0 ->
	    Msgs
=======
      #offline_msg{us = US} = Msg ->
	  receive_all(US, [Msg | Msgs], DBType)
      after 0 ->
		case DBType of
		  mnesia -> Msgs;
		  odbc -> lists:reverse(Msgs)
		end
>>>>>>> upstream/master
    end.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(offline_message_hook, HostB,
			  ?MODULE, store_packet, 50),
<<<<<<< HEAD
    ejabberd_hooks:delete(resend_offline_messages_hook, HostB,
			  ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, HostB,
=======
    ejabberd_hooks:delete(resend_offline_messages_hook,
			  Host, ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
>>>>>>> upstream/master
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_local_features, HostB, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(webadmin_page_host, HostB,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, HostB,
			  ?MODULE, webadmin_user, 50),
<<<<<<< HEAD
    ejabberd_hooks:delete(webadmin_user_parse_query, HostB,
                          ?MODULE, webadmin_user_parse_query, 50),
=======
    ejabberd_hooks:delete(webadmin_user_parse_query, Host,
			  ?MODULE, webadmin_user_parse_query, 50),
>>>>>>> upstream/master
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    {wait, Proc}.

get_sm_features(Acc, _From, _To, <<>>, _Lang) ->
    Feats = case Acc of
		{result, I} -> I;
		_ -> []
	    end,
    {result, Feats ++ [?NS_MSGOFFLINE]};

get_sm_features(_Acc, _From, _To, ?NS_MSGOFFLINE, _Lang) ->
    %% override all lesser features...
    {result, []};

get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.


store_packet(From, To, Packet) ->
<<<<<<< HEAD
    Type = exmpp_stanza:get_type(Packet),
    if
	(Type /= <<"error">>) and (Type /= <<"groupchat">>) and
	(Type /= <<"headline">>) ->
	    case check_event_chatstates(From, To, Packet) of
		true ->
            LUser = exmpp_jid:prep_node(To),
            LServer = exmpp_jid:prep_domain(To),
		    TimeStamp = make_timestamp(),
		    Expire = find_x_expire(TimeStamp, Packet#xmlel.children),
		    gen_mod:get_module_proc_existing(LServer, ?PROCNAME) !
			#offline_msg{user_host = {LUser, LServer},
				     timestamp = TimeStamp,
				     expire = Expire,
				     from = From,
				     to = To,
				     packet = stanza_to_store(Packet)},
		    stop;
		_ ->
		    ok
	    end;
	true ->
	    ok
    end.

%% Check if the packet has any content about XEP-0022 or XEP-0085
check_event_chatstates(From, To, Packet) ->
    case find_x_event_chatstates(Packet#xmlel.children, {false, false, false}) of
	%% There wasn't any x:event or chatstates subelements
	{false, false, _} ->
	    true;
	%% There a chatstates subelement and other stuff, but no x:event
	{false, CEl, true} when CEl /= false ->
  	    true;
	%% There was only a subelement: a chatstates
	{false, CEl, false} when CEl /= false ->
	    %% Don't allow offline storage
	    false;
	%% There was an x:event element, and maybe also other stuff
	{El, _, _} when El /= false->
	    case exmpp_xml:get_element(El, 'id') of
		undefined ->
		    case exmpp_xml:get_element(El, 'offline') of
			undefined ->
			    true;
			_ ->
			    ID = case exmpp_stanza:get_id(Packet) of
				     undefined ->
					 #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'id'};
				     S ->
					 #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'id',
					   children = [#xmlcdata{cdata =
					       S}]}
				 end,
			    X = #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'x', children =
			      [ID, #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'offline'}]},
			    ejabberd_router:route(
			      To, From, exmpp_xml:set_children(Packet, [X])),
			    true
		    end;
		_ ->
		    false
	    end
    end.

%% Check if the packet has subelements about XEP-0022, XEP-0085 or other
find_x_event_chatstates([], Res) ->
    Res;
find_x_event_chatstates([#xmlel{ns = ?NS_MESSAGE_EVENT} = El | Els], {_, B, C}) ->
    find_x_event_chatstates(Els, {El, B, C});
find_x_event_chatstates([#xmlel{ns = ?NS_CHATSTATES} = El | Els], {A, _, C}) ->
    find_x_event_chatstates(Els, {A, El, C});
find_x_event_chatstates([#xmlcdata{} = _ | Els], {A, B, C}) ->
    find_x_event_chatstates(Els, {A, B, C});
find_x_event_chatstates([_ | Els], {A, B, _}) ->
    find_x_event_chatstates(Els, {A, B, true}).

find_x_expire(_, []) ->
    0;
find_x_expire(TimeStamp, [#xmlel{ns = ?NS_MESSAGE_EXPIRE} = El | _Els]) ->
    Val = exmpp_xml:get_attribute_as_list(El, <<"seconds">>, ""),
    case catch list_to_integer(Val) of
	{'EXIT', _} ->
	    0;
	Int when Int > 0 ->
	    TimeStamp + Int;
	_ ->
	    0
    end;
find_x_expire(TimeStamp, [_ | Els]) ->
    find_x_expire(TimeStamp, Els).


%% @spec(User::string(), Server::string()) -> ok
resend_offline_messages(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(list_to_binary(User)),
	LServer = exmpp_stringprep:nameprep(list_to_binary(Server)),
	US = {LUser, LServer},
	F = fun() ->
		Rs = gen_storage:read(LServer, offline_msg, US, write),
		gen_storage:delete(LServer, {offline_msg, US}),
		Rs
	    end,
    case gen_storage:transaction(LServer, offline_msg, F) of
	    {atomic, Rs} ->
		lists:foreach(
		  fun(R) ->
			  Packet = store_to_stanza(R#offline_msg.packet),
			  TimeXml = timestamp_to_xml(R#offline_msg.timestamp, Server),
			  TimeXml91 = timestamp_to_xml(R#offline_msg.timestamp),
			  ejabberd_sm !
			      {route,
			       R#offline_msg.from,
			       R#offline_msg.to,
			       exmpp_xml:append_children(
				 Packet,
				 [TimeXml, TimeXml91]
				)}
		  end,
		  lists:keysort(#offline_msg.timestamp, Rs));
	    _ ->
		ok
	end
    catch
	_ ->
	    ok
    end.

%% @spec(Ls::list(), User::binary(), Server::binary()) -> list()
pop_offline_messages(Ls, User, Server)
        when is_binary(User), is_binary(Server) ->
    try
	US = {User, Server},
	F = fun() ->
		Rs = gen_storage:read(Server, offline_msg, US, write),
		gen_storage:delete(Server, {offline_msg, US}),
		Rs
	    end,
    case gen_storage:transaction(Server, offline_msg, F) of
	    {atomic, Rs} ->
	    TS = make_timestamp(),
		Ls ++ lists:map(
			fun(R) ->
				Packet = store_to_stanza(R#offline_msg.packet),
				TimeXml = timestamp_to_xml(R#offline_msg.timestamp, Server),
				TimeXml91 = timestamp_to_xml(R#offline_msg.timestamp),
				{route,
				 R#offline_msg.from,
				 R#offline_msg.to,
			       exmpp_xml:append_children(
				 Packet,
				 [TimeXml, TimeXml91]
				)}
			end,
			lists:filter(
			  fun(R) ->
				case R#offline_msg.expire of
				    0 ->
					true;
				    TimeStamp ->
					TS < TimeStamp
				end
			  end,
			  lists:keysort(#offline_msg.timestamp, Rs)));
	    _ ->
		Ls
	end
    catch
	_ ->
	    Ls
=======
    Type = xml:get_tag_attr_s(<<"type">>, Packet),
    if (Type /= <<"error">>) and (Type /= <<"groupchat">>)
	 and (Type /= <<"headline">>) ->
	   case check_event(From, To, Packet) of
	     true ->
		 #jid{luser = LUser, lserver = LServer} = To,
		 TimeStamp = now(),
		 #xmlel{children = Els} = Packet,
		 Expire = find_x_expire(TimeStamp, Els),
		 gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME) !
		   #offline_msg{us = {LUser, LServer},
				timestamp = TimeStamp, expire = Expire,
				from = From, to = To, packet = Packet},
		 stop;
	     _ -> ok
	   end;
       true -> ok
    end.

%% Check if the packet has any content about XEP-0022 or XEP-0085
check_event(From, To, Packet) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} =
	Packet,
    case find_x_event(Els) of
      false -> true;
      El ->
	  case xml:get_subtag(El, <<"id">>) of
	    false ->
		case xml:get_subtag(El, <<"offline">>) of
		  false -> true;
		  _ ->
		      ID = case xml:get_tag_attr_s(<<"id">>, Packet) of
			     <<"">> ->
				 #xmlel{name = <<"id">>, attrs = [],
					children = []};
			     S ->
				 #xmlel{name = <<"id">>, attrs = [],
					children = [{xmlcdata, S}]}
			   end,
		      ejabberd_router:route(To, From,
					    #xmlel{name = Name, attrs = Attrs,
						   children =
						       [#xmlel{name = <<"x">>,
							       attrs =
								   [{<<"xmlns">>,
								     ?NS_EVENT}],
							       children =
								   [ID,
								    #xmlel{name
									       =
									       <<"offline">>,
									   attrs
									       =
									       [],
									   children
									       =
									       []}]}]}),
		      true
		end;
	    _ -> false
	  end
    end.

%% Check if the packet has subelements about XEP-0022, XEP-0085 or other
find_x_event([]) -> false;
find_x_event([{xmlcdata, _} | Els]) ->
    find_x_event(Els);
find_x_event([El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_EVENT -> El;
      _ -> find_x_event(Els)
    end.

find_x_expire(_, []) -> never;
find_x_expire(TimeStamp, [{xmlcdata, _} | Els]) ->
    find_x_expire(TimeStamp, Els);
find_x_expire(TimeStamp, [El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_EXPIRE ->
	  Val = xml:get_tag_attr_s(<<"seconds">>, El),
	  case catch jlib:binary_to_integer(Val) of
	    {'EXIT', _} -> never;
	    Int when Int > 0 ->
		{MegaSecs, Secs, MicroSecs} = TimeStamp,
		S = MegaSecs * 1000000 + Secs + Int,
		MegaSecs1 = S div 1000000,
		Secs1 = S rem 1000000,
		{MegaSecs1, Secs1, MicroSecs};
	    _ -> never
	  end;
      _ -> find_x_expire(TimeStamp, Els)
    end.

resend_offline_messages(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun () ->
		Rs = mnesia:wread({offline_msg, US}),
		mnesia:delete({offline_msg, US}),
		Rs
	end,
    case mnesia:transaction(F) of
      {atomic, Rs} ->
	  lists:foreach(fun (R) ->
				#xmlel{name = Name, attrs = Attrs,
				       children = Els} =
				    R#offline_msg.packet,
				ejabberd_sm !
				  {route, R#offline_msg.from, R#offline_msg.to,
				   #xmlel{name = Name, attrs = Attrs,
					  children =
					      Els ++
						[jlib:timestamp_to_xml(calendar:now_to_universal_time(R#offline_msg.timestamp))]}}
			end,
			lists:keysort(#offline_msg.timestamp, Rs));
      _ -> ok
    end.

pop_offline_messages(Ls, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    pop_offline_messages(Ls, LUser, LServer,
			 gen_mod:db_type(LServer, ?MODULE)).

pop_offline_messages(Ls, LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		Rs = mnesia:wread({offline_msg, US}),
		mnesia:delete({offline_msg, US}),
		Rs
	end,
    case mnesia:transaction(F) of
      {atomic, Rs} ->
	  TS = now(),
	  Ls ++
	    lists:map(fun (R) ->
			      offline_msg_to_route(LServer, R)
		      end,
		      lists:filter(fun (R) ->
					   case R#offline_msg.expire of
					     never -> true;
					     TimeStamp -> TS < TimeStamp
					   end
				   end,
				   lists:keysort(#offline_msg.timestamp, Rs)));
      _ -> Ls
    end;
pop_offline_messages(Ls, LUser, LServer, odbc) ->
    EUser = ejabberd_odbc:escape(LUser),
    case odbc_queries:get_and_del_spool_msg_t(LServer,
					      EUser)
	of
      {atomic, {selected, [<<"username">>, <<"xml">>], Rs}} ->
	  Ls ++
	    lists:flatmap(fun ([_, XML]) ->
				  case xml_stream:parse_element(XML) of
				    {error, _Reason} ->
                                          [];
				    El ->
                                          case offline_msg_to_route(LServer, El) of
                                              error ->
                                                  [];
                                              RouteMsg ->
                                                  [RouteMsg]
                                          end
				  end
			  end,
			  Rs);
      _ -> Ls
>>>>>>> upstream/master
    end.

remove_expired_messages(Server) ->
    LServer = jlib:nameprep(Server),
    remove_expired_messages(LServer,
			    gen_mod:db_type(LServer, ?MODULE)).

<<<<<<< HEAD
remove_expired_messages() ->
    TimeStamp = make_timestamp(),
    lists:foreach(
      fun(HostB) ->
	      F = fun() ->
			  gen_storage:delete_where(
			    HostB, offline_msg,
			    [{'andalso',
			      {'=/=', expire, 0},
			      {'<', expire, TimeStamp}}])
		  end,
	      gen_storage:transaction(HostB, offline_msg, F)
      end, gen_storage:all_table_hosts(offline_msg)).

remove_old_messages(Days) ->
    Timestamp = make_timestamp() - 1000 * 60 * 60 * 24 * Days,
    lists:foreach(
      fun(HostB) ->
	      F = fun() ->
			  gen_storage:delete_where(HostB, offline_msg,
						   [{'<', timestamp, Timestamp}])
		  end,
	      {atomic, _} = gen_storage:transaction(HostB, offline_msg, F)
      end, gen_storage:all_table_hosts(offline_msg)).

%% @spec(User::binary(), Server::binary()) -> ok
remove_user(User, Server) when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	F = fun() ->
		    gen_storage:delete(LServer, {offline_msg, US})
	    end,
	gen_storage:transaction(LServer, offline_msg, F)
    catch
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_table(global, Storage) ->
    [update_table(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_table(Host, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      Host, offline_msg,
      [{offline_msg, [us, timestamp, expire, from, to, packet],
	%% The field name 'us' changes to 'user_host',
	%% but its position in the erlang record is the same,
	%% so we can refer to it using the new field name 'user_host'.
	fun(#offline_msg{user_host = {US_U, US_S},
	                 timestamp = {TsMegaSecs, TsSecs, TsMicroSecs},
		         expire = Expire,
		         from = From,
		         to = To,
			 packet = Packet} = OM) ->
		%% Convert "" to undefined in JIDs.
		US_U1 = convert_jid_to_exmpp(US_U),
		US_S1 = convert_jid_to_exmpp(US_S),
		From1 = jlib:from_old_jid(From),
		To1 = jlib:from_old_jid(To),
		Expire1 = case Expire of
			      never ->
				  0;
			      Ts ->
				  Ts
			  end,
		PacketXmlel = exmpp_xml:xmlelement_to_xmlel(
				Packet, [?DEFAULT_NS], ?PREFIXED_NS),
	        Packet1 = stanza_to_store(PacketXmlel),
		OM#offline_msg{user_host = {US_U1, US_S1},
		               timestamp = TsMegaSecs * 1000000000 + TsSecs * 1000 + TsMicroSecs div 1000,
		               expire = Expire1,
		               from = From1,
		               to = To1,
			       packet = Packet1}
	end}]);

update_table(Host, odbc) ->
    gen_storage_migration:migrate_odbc(
      Host, [offline_msg],
      [{"spool", ["username", "xml", "seq"],
	fun(_, Username, XmlS, _Seq) ->
		[Xmlel] = Els = exmpp_xml:parse_document(XmlS, [names_as_atom]),
		From = jlib:short_prepd_jid(
			 exmpp_jid:parse(
			   exmpp_stanza:get_sender(Xmlel))),
		Expire = find_x_expire(0, Els),
		Timestamp = find_x_timestamp(Els),
		[#offline_msg{user_host = {Username, Host},
			      timestamp = Timestamp,
			      expire = Expire,
			      from = From,
			      to = {Username, Host, ""},
			      packet = XmlS}]
	end}]).

convert_jid_to_exmpp("") -> undefined;
convert_jid_to_exmpp(V)  -> list_to_binary(V).

%% Return the current timestamp in milliseconds
make_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.

timestamp_to_now(Timestamp) ->
    MegaSecs = Timestamp div 1000000000,
    Secs = (Timestamp rem 1000000000) div 1000,
    MicroSecs = (Timestamp rem 1000) * 1000,
    {MegaSecs, Secs, MicroSecs}.

timestamp_to_isostring(Timestamp) ->
    {Date, Time} = calendar:now_to_universal_time(timestamp_to_now(Timestamp)),
    Milliseconds = (Timestamp rem 1000),
    UniversalTimeSubsecond = {Date, Time, {milliseconds, Milliseconds}},
    {TimeStr, ZoneStr} = jlib:timestamp_to_iso(UniversalTimeSubsecond, utc),
    TimeStr ++ ZoneStr.

timestamp_to_xml(Timestamp, Server) ->
    {Date, Time} = calendar:now_to_universal_time(timestamp_to_now(Timestamp)),
    Milliseconds = (Timestamp rem 1000),
    UniversalTimeSubsecond = {Date, Time, {milliseconds, Milliseconds}},
    jlib:timestamp_to_xml(
	UniversalTimeSubsecond,
	utc,
	exmpp_jid:make("", Server, ""),
	"Offline Storage").

%% TODO: Delete once XEP-0091 is Obsolete
timestamp_to_xml(Timestamp) ->
    jlib:timestamp_to_xml(
	calendar:now_to_universal_time(
	    timestamp_to_now(Timestamp))).

find_x_timestamp([]) ->
    make_timestamp();
find_x_timestamp([{xmlcdata, _} | Els]) ->
    find_x_timestamp(Els);

find_x_timestamp([#xmlel{ns = ?NS_DELAY} = El | Els]) ->
    Stamp = exmpp_xml:get_attribute_as_list(El, <<"stamp">>, ""),
    case jlib:datetime_string_to_timestamp(Stamp) of
	undefined -> find_x_timestamp(Els);
	{MegaSecs, Secs, _MicroSecs} -> MegaSecs * 1000000000 + Secs * 1000
    end;
find_x_timestamp([_ | Els]) ->
    find_x_timestamp(Els).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(
      fun(#offline_msg{from=From, to=To, packet=PacketStored}) ->
	      Packet = store_to_stanza(PacketStored),
	      ErrText = "Your contact offline message queue is full. The message has been discarded.",
	      Error = exmpp_stanza:error(Packet#xmlel.ns, 'resource-constraint',
		{"en", ErrText}),
	      Err = exmpp_stanza:reply_with_error(Packet, Error),
	      ejabberd_router:route(
		To,
		From, Err)
      end, Msgs).

=======
remove_expired_messages(_LServer, mnesia) ->
    TimeStamp = now(),
    F = fun () ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(fun (Rec, _Acc) ->
				     case Rec#offline_msg.expire of
				       never -> ok;
				       TS ->
					   if TS < TimeStamp ->
						  mnesia:delete_object(Rec);
					      true -> ok
					   end
				     end
			     end,
			     ok, offline_msg)
	end,
    mnesia:transaction(F);
remove_expired_messages(_LServer, odbc) -> {atomic, ok}.

remove_old_messages(Days, Server) ->
    LServer = jlib:nameprep(Server),
    remove_old_messages(Days, LServer,
			gen_mod:db_type(LServer, ?MODULE)).

remove_old_messages(Days, _LServer, mnesia) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    S = MegaSecs * 1000000 + Secs - 60 * 60 * 24 * Days,
    MegaSecs1 = S div 1000000,
    Secs1 = S rem 1000000,
    TimeStamp = {MegaSecs1, Secs1, 0},
    F = fun () ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(fun (#offline_msg{timestamp = TS} = Rec,
				  _Acc)
				     when TS < TimeStamp ->
				     mnesia:delete_object(Rec);
				 (_Rec, _Acc) -> ok
			     end,
			     ok, offline_msg)
	end,
    mnesia:transaction(F);
remove_old_messages(_Days, _LServer, odbc) ->
    {atomic, ok}.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    remove_user(LUser, LServer,
		gen_mod:db_type(LServer, ?MODULE)).

remove_user(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:delete({offline_msg, US}) end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_spool_msg(LServer, Username).

jid_to_binary(#jid{user = U, server = S, resource = R,
                   luser = LU, lserver = LS, lresource = LR}) ->
    #jid{user = iolist_to_binary(U),
         server = iolist_to_binary(S),
         resource = iolist_to_binary(R),
         luser = iolist_to_binary(LU),
         lserver = iolist_to_binary(LS),
         lresource = iolist_to_binary(LR)}.

update_table() ->
    Fields = record_info(fields, offline_msg),
    case mnesia:table_info(offline_msg, attributes) of
        Fields ->
            ejabberd_config:convert_table_to_binary(
              offline_msg, Fields, bag,
              fun(#offline_msg{us = {U, _}}) -> U end,
              fun(#offline_msg{us = {U, S},
                               from = From,
                               to = To,
                               packet = El} = R) ->
                      R#offline_msg{us = {iolist_to_binary(U),
                                          iolist_to_binary(S)},
                                    from = jid_to_binary(From),
                                    to = jid_to_binary(To),
                                    packet = xml:to_xmlel(El)}
              end);
        _ ->
            ?INFO_MSG("Recreating offline_msg table", []),
            mnesia:transform_table(offline_msg, ignore, Fields)
    end.

%% Helper functions:

%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(fun (#offline_msg{from = From, to = To,
				    packet = Packet}) ->
			  ErrText = <<"Your contact offline message queue is "
				      "full. The message has been discarded.">>,
			  Lang = xml:get_tag_attr_s(<<"xml:lang">>, Packet),
			  Err = jlib:make_error_reply(Packet,
						      ?ERRT_RESOURCE_CONSTRAINT(Lang,
										ErrText)),
			  ejabberd_router:route(To, From, Err)
		  end,
		  Msgs).
>>>>>>> upstream/master

webadmin_page(_, Host,
	      #request{us = _US, path = [<<"user">>, U, <<"queue">>],
		       q = Query, lang = Lang} =
		  _Request) ->
    Res = user_queue(U, Host, Query, Lang), {stop, Res};
webadmin_page(Acc, _, _) -> Acc.

get_offline_els(LUser, LServer) ->
    get_offline_els(LUser, LServer, gen_mod:db_type(LServer, ?MODULE)).

get_offline_els(LUser, LServer, mnesia) ->
    Msgs = read_all_msgs(LUser, LServer, mnesia),
    lists:map(
      fun(Msg) ->
              {route, From, To, Packet} = offline_msg_to_route(LServer, Msg),
              jlib:replace_from_to(From, To, Packet)
      end, Msgs);
get_offline_els(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(LServer,
                                       [<<"select xml from spool where username='">>,
                                        Username, <<"' order by seq;">>]) of
        {selected, [<<"xml">>], Rs} ->
            lists:flatmap(
              fun([XML]) ->
                      case xml_stream:parse_element(XML) of
                          #xmlel{} = El ->
                              case offline_msg_to_route(LServer, El) of
                                  {route, _, _, NewEl} ->
                                      [NewEl];
                                  error ->
                                      []
                              end;
                          _ ->
                              []
                      end
              end, Rs);
        _ ->
            []
    end.

offline_msg_to_route(LServer, #offline_msg{} = R) ->
    El = #xmlel{children = Els} = R#offline_msg.packet,
    {route, R#offline_msg.from, R#offline_msg.to,
     El#xmlel{children =
                  Els ++
                  [jlib:timestamp_to_xml(
                     calendar:now_to_universal_time(
                       R#offline_msg.timestamp),
                     utc,
                     jlib:make_jid(<<"">>, LServer, <<"">>),
                     <<"Offline Storage">>),
                   jlib:timestamp_to_xml(
                     calendar:now_to_universal_time(
                       R#offline_msg.timestamp))]}};
offline_msg_to_route(_LServer, #xmlel{} = El) ->
    To = jlib:string_to_jid(xml:get_tag_attr_s(<<"to">>, El)),
    From = jlib:string_to_jid(xml:get_tag_attr_s(<<"from">>, El)),
    if (To /= error) and (From /= error) ->
            {route, From, To, El};
       true ->
            error
    end.

read_all_msgs(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    lists:keysort(#offline_msg.timestamp,
		  mnesia:dirty_read({offline_msg, US}));
read_all_msgs(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select xml from spool  where username='">>,
					Username, <<"'  order by seq;">>])
	of
      {selected, [<<"xml">>], Rs} ->
	  lists:flatmap(fun ([XML]) ->
				case xml_stream:parse_element(XML) of
				  {error, _Reason} -> [];
				  El -> [El]
				end
			end,
			Rs);
      _ -> []
    end.

format_user_queue(Msgs, mnesia) ->
    lists:map(fun (#offline_msg{timestamp = TimeStamp,
				from = From, to = To,
				packet =
				    #xmlel{name = Name, attrs = Attrs,
					   children = Els}} =
		       Msg) ->
		      ID = jlib:encode_base64((term_to_binary(Msg))),
		      {{Year, Month, Day}, {Hour, Minute, Second}} =
			  calendar:now_to_local_time(TimeStamp),
		      Time =
			  iolist_to_binary(io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
							 [Year, Month, Day,
							  Hour, Minute,
							  Second])),
		      SFrom = jlib:jid_to_string(From),
		      STo = jlib:jid_to_string(To),
		      Attrs2 = jlib:replace_from_to_attrs(SFrom, STo, Attrs),
		      Packet = #xmlel{name = Name, attrs = Attrs2,
				      children = Els},
		      FPacket = ejabberd_web_admin:pretty_print_xml(Packet),
		      ?XE(<<"tr">>,
			  [?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?INPUT(<<"checkbox">>, <<"selected">>, ID)]),
			   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], Time),
			   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], SFrom),
			   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], STo),
			   ?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?XC(<<"pre">>, FPacket)])])
	      end,
	      Msgs);
format_user_queue(Msgs, odbc) ->
    lists:map(fun (#xmlel{} = Msg) ->
		      ID = jlib:encode_base64((term_to_binary(Msg))),
		      Packet = Msg,
		      FPacket = ejabberd_web_admin:pretty_print_xml(Packet),
		      ?XE(<<"tr">>,
			  [?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?INPUT(<<"checkbox">>, <<"selected">>, ID)]),
			   ?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?XC(<<"pre">>, FPacket)])])
	      end,
	      Msgs).

user_queue(User, Server, Query, Lang) ->
<<<<<<< HEAD
    ServerB = list_to_binary(Server),
	US0 = {
	  exmpp_stringprep:nodeprep(list_to_binary(User)),
	  exmpp_stringprep:nameprep(list_to_binary(Server))
	},
    {US, Res, MsgsAll} = try
	{
	  US0,
	  user_queue_parse_query(US0, Query),
	  lists:keysort(#offline_msg.timestamp,
			 gen_storage:dirty_read(ServerB, {offline_msg, US0}))
	}
    catch
	_ ->
	    {{"invalid", "invalid"}, [], []}
    end,
    Msgs = get_messages_subset(User, Server, MsgsAll),
    FMsgs =
	lists:map(
	  fun(#offline_msg{timestamp = TimeStamp, from = From, to = To,
			   packet = PacketStored} = Msg) ->
		  ID = jlib:encode_base64(binary_to_list(term_to_binary(Msg))),
		  Time = timestamp_to_isostring(TimeStamp),
		  SFrom = exmpp_jid:to_list(From),
		  STo = exmpp_jid:to_list(To),
		  Packet = store_to_stanza(PacketStored),
		  Packet1 = exmpp_stanza:set_jids(Packet, SFrom, STo),
		  FPacket = exmpp_xml:node_to_list(
		    exmpp_xml:indent_document(Packet1, <<"  ">>),
		    [?DEFAULT_NS], ?PREFIXED_NS),
		  ?XE("tr",
		      [?XAE("td", [?XMLATTR(<<"class">>, <<"valign">>)], [?INPUT("checkbox", "selected", ID)]),
		       ?XAC("td", [?XMLATTR(<<"class">>, <<"valign">>)], Time),
		       ?XAC("td", [?XMLATTR(<<"class">>, <<"valign">>)], SFrom),
		       ?XAC("td", [?XMLATTR(<<"class">>, <<"valign">>)], STo),
		       ?XAE("td", [?XMLATTR(<<"class">>, <<"valign">>)], [?XC("pre", FPacket)])]
		     )
	  end, Msgs),
    [?XC("h1", io_lib:format(?T("~s's Offline Messages Queue"),
			     [us_to_list(US)]))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    nothing -> []
	end ++
	[?XAE("form", [?XMLATTR(<<"action">>, <<"">>), ?XMLATTR(<<"method">>, <<"post">>)],
	      [?XE("table",
		   [?XE("thead",
			[?XE("tr",
			     [?X("td"),
			      ?XCT("td", "Time"),
			      ?XCT("td", "From"),
			      ?XCT("td", "To"),
			      ?XCT("td", "Packet")
			     ])]),
		    ?XE("tbody",
			if
			    FMsgs == [] ->
				[?XE("tr",
				     [?XAC("td", [?XMLATTR(<<"colspan">>, <<"4">>)], " ")]
				    )];
			    true ->
				FMsgs
			end
		       )]),
	       ?BR,
	       ?INPUTT("submit", "delete", "Delete Selected")
	      ])].

user_queue_parse_query(US, Query) ->
    case lists:keysearch("delete", 1, Query) of
	{value, _} ->
	    {_, LServer} = US,
	    Msgs = lists:keysort(#offline_msg.timestamp,
				 gen_storage:dirty_read(LServer, {offline_msg, US})),
	    F = fun() ->
			lists:foreach(
			  fun(Msg) ->
				  ID = jlib:encode_base64(
					 binary_to_list(term_to_binary(Msg))),
				  case lists:member({"selected", ID}, Query) of
				      true ->
					  gen_storage:delete_object(LServer, Msg);
				      false ->
					  ok
				  end
			  end, Msgs)
		end,
	    gen_storage:transaction(LServer, offline_msg, F),
	    ok;
	false ->
	    nothing
    end.

us_to_list({User, Server}) ->
    exmpp_jid:to_list(User, Server).

%% @spec (User::binary(), Host::binary()) -> integer()
get_queue_length(User, Host) ->
    gen_storage:dirty_count_records(Host, offline_msg, [{'=', user_host, {User, Host}}]).

get_messages_subset(User, Host, MsgsAll) ->
    Access = gen_mod:get_module_opt(Host, ?MODULE, access_max_user_messages,
				    max_user_offline_messages),
    MaxOfflineMsgs = case get_max_user_messages(Access, User, Host) of
			 Number when is_integer(Number) -> Number;
			 _ -> 100
		     end,
    Length = length(MsgsAll),
    get_messages_subset2(MaxOfflineMsgs, Length, MsgsAll).

get_messages_subset2(Max, Length, MsgsAll) when Length =< Max*2 ->
    MsgsAll;
get_messages_subset2(Max, Length, MsgsAll) ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN, Msgs2),
    NoJID = exmpp_jid:make("...", "...", ""),
    TimeStamp = make_timestamp(),
    IntermediateMsg = #offline_msg{timestamp = TimeStamp, from = NoJID, to = NoJID,
				   packet = <<"...">>},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN.

webadmin_user(Acc, User, Server, Lang) ->
    LUser = list_to_binary(exmpp_stringprep:nodeprep(User)),
    LServer = list_to_binary(exmpp_stringprep:nameprep(Server)),
    QueueLen = get_queue_length(LUser, LServer),
    FQueueLen = [?AC("queue/", integer_to_list(QueueLen))],
    Acc ++ [?XCT("h3", "Offline Messages:")] ++ FQueueLen ++ [?C(" "), ?INPUTT("submit", "removealloffline", "Remove All Offline Messages")].

webadmin_user_parse_query(_, "removealloffline", User, Server, _Query) ->
    UserB = list_to_binary(User),
    ServerB = list_to_binary(Server),
    US = {UserB, ServerB},
    F = fun() ->
            gen_storage:write_lock_table(ServerB, offline_msg),
            lists:foreach(
              fun(Msg) ->
                      gen_storage:delete_object(ServerB, Msg)
              end, gen_storage:read(ServerB, {offline_msg, US}))
        end,
    case gen_storage:transaction(ServerB, offline_msg, F) of
         {aborted, Reason} ->
            ?ERROR_MSG("Failed to remove offline messages: ~p", [Reason]),
            {stop, error};
         {atomic, ok} ->
            ?INFO_MSG("Removed all offline messages for ~s@~s", [User, Server]),
            {stop, ok}
=======
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    DBType = gen_mod:db_type(LServer, ?MODULE),
    Res = user_queue_parse_query(LUser, LServer, Query,
				 DBType),
    MsgsAll = read_all_msgs(LUser, LServer, DBType),
    Msgs = get_messages_subset(US, Server, MsgsAll,
			       DBType),
    FMsgs = format_user_queue(Msgs, DBType),
    [?XC(<<"h1">>,
	 list_to_binary(io_lib:format(?T(<<"~s's Offline Messages Queue">>),
                                      [us_to_list(US)])))]
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?XE(<<"table">>,
		   [?XE(<<"thead">>,
			[?XE(<<"tr">>,
			     [?X(<<"td">>), ?XCT(<<"td">>, <<"Time">>),
			      ?XCT(<<"td">>, <<"From">>),
			      ?XCT(<<"td">>, <<"To">>),
			      ?XCT(<<"td">>, <<"Packet">>)])]),
		    ?XE(<<"tbody">>,
			if FMsgs == [] ->
			       [?XE(<<"tr">>,
				    [?XAC(<<"td">>, [{<<"colspan">>, <<"4">>}],
					  <<" ">>)])];
			   true -> FMsgs
			end)]),
	       ?BR,
	       ?INPUTT(<<"submit">>, <<"delete">>,
		       <<"Delete Selected">>)])].

user_queue_parse_query(LUser, LServer, Query, mnesia) ->
    US = {LUser, LServer},
    case lists:keysearch(<<"delete">>, 1, Query) of
      {value, _} ->
	  Msgs = lists:keysort(#offline_msg.timestamp,
			       mnesia:dirty_read({offline_msg, US})),
	  F = fun () ->
		      lists:foreach(fun (Msg) ->
					    ID =
						jlib:encode_base64((term_to_binary(Msg))),
					    case lists:member({<<"selected">>,
							       ID},
							      Query)
						of
					      true -> mnesia:delete_object(Msg);
					      false -> ok
					    end
				    end,
				    Msgs)
	      end,
	  mnesia:transaction(F),
	  ok;
      false -> nothing
    end;
user_queue_parse_query(LUser, LServer, Query, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case lists:keysearch(<<"delete">>, 1, Query) of
      {value, _} ->
	  Msgs = case catch ejabberd_odbc:sql_query(LServer,
						    [<<"select xml, seq from spool  where username='">>,
						     Username,
						     <<"'  order by seq;">>])
		     of
		   {selected, [<<"xml">>, <<"seq">>], Rs} ->
		       lists:flatmap(fun ([XML, Seq]) ->
					     case xml_stream:parse_element(XML)
						 of
					       {error, _Reason} -> [];
					       El -> [{El, Seq}]
					     end
				     end,
				     Rs);
		   _ -> []
		 end,
	  F = fun () ->
		      lists:foreach(fun ({Msg, Seq}) ->
					    ID =
						jlib:encode_base64((term_to_binary(Msg))),
					    case lists:member({<<"selected">>,
							       ID},
							      Query)
						of
					      true ->
						  SSeq =
						      ejabberd_odbc:escape(Seq),
						  catch
						    ejabberd_odbc:sql_query(LServer,
									    [<<"delete from spool  where username='">>,
									     Username,
									     <<"'  and seq='">>,
									     SSeq,
									     <<"';">>]);
					      false -> ok
					    end
				    end,
				    Msgs)
	      end,
	  mnesia:transaction(F),
	  ok;
      false -> nothing
    end.

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, <<"">>}).

get_queue_length(LUser, LServer) ->
    get_queue_length(LUser, LServer,
		     gen_mod:db_type(LServer, ?MODULE)).

get_queue_length(LUser, LServer, mnesia) ->
    length(mnesia:dirty_read({offline_msg,
			       {LUser, LServer}}));
get_queue_length(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select count(*) from spool  where username='">>,
					Username, <<"';">>])
	of
      {selected, [_], [[SCount]]} ->
	  jlib:binary_to_integer(SCount);
      _ -> 0
    end.

get_messages_subset(User, Host, MsgsAll, DBType) ->
    Access = gen_mod:get_module_opt(Host, ?MODULE, access_max_user_messages,
                                    fun(A) when is_atom(A) -> A end,
				    max_user_offline_messages),
    MaxOfflineMsgs = case get_max_user_messages(Access,
						User, Host)
			 of
		       Number when is_integer(Number) -> Number;
		       _ -> 100
		     end,
    Length = length(MsgsAll),
    get_messages_subset2(MaxOfflineMsgs, Length, MsgsAll,
			 DBType).

get_messages_subset2(Max, Length, MsgsAll, _DBType)
    when Length =< Max * 2 ->
    MsgsAll;
get_messages_subset2(Max, Length, MsgsAll, mnesia) ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN,
			      Msgs2),
    NoJID = jlib:make_jid(<<"...">>, <<"...">>, <<"">>),
    IntermediateMsg = #offline_msg{timestamp = now(),
				   from = NoJID, to = NoJID,
				   packet =
				       #xmlel{name = <<"...">>, attrs = [],
					      children = []}},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN;
get_messages_subset2(Max, Length, MsgsAll, odbc) ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN,
			      Msgs2),
    IntermediateMsg = #xmlel{name = <<"...">>, attrs = [],
			     children = []},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN.

webadmin_user(Acc, User, Server, Lang) ->
    QueueLen = get_queue_length(jlib:nodeprep(User),
				jlib:nameprep(Server)),
    FQueueLen = [?AC(<<"queue/">>,
		     (iolist_to_binary(integer_to_list(QueueLen))))],
    Acc ++
      [?XCT(<<"h3">>, <<"Offline Messages:">>)] ++
	FQueueLen ++
	  [?C(<<" ">>),
	   ?INPUTT(<<"submit">>, <<"removealloffline">>,
		   <<"Remove All Offline Messages">>)].

delete_all_msgs(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    delete_all_msgs(LUser, LServer,
		    gen_mod:db_type(LServer, ?MODULE)).

delete_all_msgs(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:write_lock_table(offline_msg),
		lists:foreach(fun (Msg) -> mnesia:delete_object(Msg)
			      end,
			      mnesia:dirty_read({offline_msg, US}))
	end,
    mnesia:transaction(F);
delete_all_msgs(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_spool_msg(LServer, Username),
    {atomic, ok}.

webadmin_user_parse_query(_, <<"removealloffline">>,
			  User, Server, _Query) ->
    case delete_all_msgs(User, Server) of
      {aborted, Reason} ->
	  ?ERROR_MSG("Failed to remove offline messages: ~p",
		     [Reason]),
	  {stop, error};
      {atomic, ok} ->
	  ?INFO_MSG("Removed all offline messages for ~s@~s",
		    [User, Server]),
	  {stop, ok}
>>>>>>> upstream/master
    end;
webadmin_user_parse_query(Acc, _Action, _User, _Server,
			  _Query) ->
    Acc.

<<<<<<< HEAD
=======
%% Returns as integer the number of offline messages for a given user
count_offline_messages(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:count_records_where(
		 LServer, "spool",
                 <<"where username='", Username/binary, "'">>) of
        {selected, [_], [[Res]]} ->
            jlib:binary_to_integer(Res);
        _ ->
            0
    end.

export(_Server) ->
    [{offline_msg,
      fun(Host, #offline_msg{us = {LUser, LServer},
                             timestamp = TimeStamp, from = From, to = To,
                             packet = Packet})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              #xmlel{name = Name, attrs = Attrs, children = Els} =
                  Packet,
              Attrs2 =
                  jlib:replace_from_to_attrs(jlib:jid_to_string(From),
                                             jlib:jid_to_string(To),
                                             Attrs),
              NewPacket = #xmlel{name = Name, attrs = Attrs2,
                                 children =
                                     Els ++
                                     [jlib:timestamp_to_xml(
                                        calendar:now_to_universal_time(TimeStamp),
                                        utc,
                                        jlib:make_jid(<<"">>,
                                                      LServer,
                                                      <<"">>),
                                        <<"Offline Storage">>),
                                      jlib:timestamp_to_xml(
                                        calendar:now_to_universal_time(TimeStamp))]},
              XML =
                  ejabberd_odbc:escape(xml:element_to_binary(NewPacket)),
              [[<<"delete from spool where username='">>, Username, <<"';">>],
               [<<"insert into spool(username, xml) values ('">>,
                Username, <<"', '">>, XML, <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, xml from spool;">>,
      fun([LUser, XML]) ->
              El = #xmlel{} = xml_stream:parse_element(XML),
              From = #jid{} = jlib:string_to_jid(
                                xml:get_attr_s(<<"from">>, El#xmlel.attrs)),
              To = #jid{} = jlib:string_to_jid(
                              xml:get_attr_s(<<"to">>, El#xmlel.attrs)),
              Stamp = xml:get_path_s(El, [{elem, <<"delay">>},
                                          {attr, <<"stamp">>}]),
              TS = case jlib:datetime_string_to_timestamp(Stamp) of
                       {_, _, _} = Now ->
                           Now;
                       undefined ->
                           now()
                   end,
              Expire = find_x_expire(TS, El#xmlel.children),
              #offline_msg{us = {LUser, LServer},
                           from = From, to = To,
                           timestamp = TS, expire = Expire}
      end}].

import(_LServer, mnesia, #offline_msg{} = Msg) ->
    mnesia:dirty_write(Msg);
import(_, _, _) ->
    pass.
>>>>>>> upstream/master
