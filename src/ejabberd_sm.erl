%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_sm).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 route/3,
<<<<<<< HEAD
	 set_session/4,
	 open_session/3,
	 open_session/4,
	 close_session/2,
=======
	 open_session/5,
	 open_session/6,
	 close_session/4,
>>>>>>> upstream/master
	 check_in_subscription/6,
	 bounce_offline_message/3,
	 disconnect_removed_user/2,
	 get_user_sessions/2,
	 get_user_resources/2,
<<<<<<< HEAD
	 set_presence/5,
	 unset_presence/4,
	 close_session_unset_presence/3,
=======
	 get_user_present_resources/2,
	 set_presence/7,
	 unset_presence/6,
	 close_session_unset_presence/5,
>>>>>>> upstream/master
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 get_vh_session_list/1,
	 get_vh_my_session_list/1,
	 get_vh_session_number/1,
	 register_iq_handler/4,
	 register_iq_handler/5,
	 unregister_iq_handler/2,
	 force_update_presence/1,
	 connected_users/0,
	 connected_users_number/0,
	 user_resources/2,
<<<<<<< HEAD
	 get_session_pid/1,
	 get_user_info/3,
	 get_user_ip/1,
	 is_existing_resource/3,
	 migrate/1
=======
	 kick_user/2,
	 get_session_pid/3,
	 get_user_info/3,
	 get_user_ip/3,
	 get_max_user_sessions/2,
	 get_all_pids/0,
	 is_existing_resource/3
>>>>>>> upstream/master
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD
=======
-include("logger.hrl").

-include("jlib.hrl").

>>>>>>> upstream/master
-include("ejabberd_commands.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("mod_privacy.hrl").

-record(session, {sid, usr, us, priority, info}).
-record(state, {}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).

% These are the namespace already declared by the stream opening. This is
% used at serialization time.
-define(DEFAULT_NS, ?NS_JABBER_CLIENT).
-define(PREFIXED_NS,
        [{?NS_XMPP, ?NS_XMPP_pfx}, {?NS_DIALBACK, ?NS_DIALBACK_pfx}]).


-define(IS_BINARY_OR_UNDEF(X),
            (is_binary(X) orelse X == 'undefined')).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-type sid() :: {erlang:timestamp(), pid()}.
-type ip() :: {inet:ip_address(), inet:port_number()} | undefined.
-type info() :: [{conn, atom()} | {ip, ip()} | {node, atom()}
                 | {oor, boolean()} | {auth_module, atom()}].
-type prio() :: undefined | integer().

-export_type([sid/0]).

start_link() ->
<<<<<<< HEAD
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% #xmlelement{} used for retro-compatibility
route(FromOld, ToOld, #xmlelement{} = PacketOld) ->
    catch throw(for_stacktrace), % To have a stacktrace.
    io:format("~nSM: old #xmlelement:~n~p~n~p~n~n",
      [PacketOld, erlang:get_stacktrace()]),
    % XXX OLD FORMAT: From, To, Packet.
    From = jlib:from_old_jid(FromOld),
    To = jlib:from_old_jid(ToOld),
    Packet = exmpp_xml:xmlelement_to_xmlel(PacketOld, [?NS_JABBER_CLIENT],
      [{?NS_XMPP, ?NS_XMPP_pfx}]),
    route(From, To, Packet);
=======
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec route(jid(), jid(), xmlel() | broadcast()) -> ok.

>>>>>>> upstream/master
route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end.

<<<<<<< HEAD
open_session(SID, JID, Info) ->
    open_session(SID, JID, undefined, Info).

open_session(SID, JID, Priority, Info) when ?IS_JID(JID) ->
    set_session(SID, JID, Priority, Info),
    check_for_sessions_to_replace(JID),
    ejabberd_hooks:run(sm_register_connection_hook, exmpp_jid:prep_domain(JID),
		       [SID, JID, Info]).
=======
-spec open_session(sid(), binary(), binary(), binary(), prio(), info()) -> ok.

open_session(SID, User, Server, Resource, Priority, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    mnesia:dirty_update_counter(session_counter,
				jlib:nameprep(Server), 1),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec open_session(sid(), binary(), binary(), binary(), info()) -> ok.

open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined, Info).

-spec close_session(sid(), binary(), binary(), binary()) -> ok.
>>>>>>> upstream/master

close_session(SID, JID ) when ?IS_JID(JID)-> 
    Info = case mnesia:dirty_read({session, SID}) of
	[] -> [];
	[#session{info=I}] -> I
    end,
    F = fun() ->
<<<<<<< HEAD
		mnesia:delete({session, SID})
	end,
    mnesia:sync_dirty(F),
    ejabberd_hooks:run(sm_remove_connection_hook, exmpp_jid:prep_domain(JID),
		       [SID, JID, Info]).

check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) 
        when is_binary(User), is_binary(Server)->
    case ejabberd_auth:is_user_exists(binary_to_list(User), binary_to_list(Server)) of
	true ->
	    Acc;
	false ->
	    {stop, false}
=======
		mnesia:delete({session, SID}),
		mnesia:dirty_update_counter(session_counter,
					    jlib:nameprep(Server), -1)
	end,
    mnesia:sync_dirty(F),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
      true -> Acc;
      false -> {stop, false}
>>>>>>> upstream/master
    end.

-spec bounce_offline_message(jid(), jid(), xmlel()) -> stop.

bounce_offline_message(From, To, Packet) ->
<<<<<<< HEAD
    Err = exmpp_stanza:reply_with_error(Packet, 'service-unavailable'),
=======
    Err = jlib:make_error_reply(Packet,
				?ERR_SERVICE_UNAVAILABLE),
>>>>>>> upstream/master
    ejabberd_router:route(To, From, Err),
    stop.

-spec disconnect_removed_user(binary(), binary()) -> ok.

disconnect_removed_user(User, Server) ->
<<<<<<< HEAD
    ejabberd_sm:route(exmpp_jid:make(),
		      exmpp_jid:make(User, 
                                      Server),
                      #xmlel{name = 'broadcast', ns = exit,
                        attrs = [?XMLATTR(<<"reason">>, <<"User removed">>)]}).

get_user_sessions(User, Server) 
  when is_binary(User), is_binary(Server) ->
    US = {User, Server},
    case ejabberd_cluster:get_node({User, Server}) of
	Node when Node == node() ->
	    catch mnesia:dirty_index_read(session, US, #session.us);
	Node ->
	    catch rpc:call(Node, mnesia, dirty_index_read,
			[session, US, #session.us], 5000)
    end.

get_user_resources(User, Server) 
  when is_binary(User), is_binary(Server) ->
    US = {User, Server},
    Ss = case ejabberd_cluster:get_node({User, Server}) of
	     Node when Node == node() ->
		 catch mnesia:dirty_index_read(session, US, #session.us);
	     Node ->
		 catch rpc:call(Node, mnesia, dirty_index_read,
				[session, US, #session.us], 5000)
	 end,
    if is_list(Ss) ->
	    [element(3, S#session.usr) || S <- clean_session_list(Ss)];
       true ->
	    []
    end.

get_user_ip(JID) when ?IS_JID(JID) ->
    USR = {LUser = exmpp_jid:prep_node(JID), 
           LServer = exmpp_jid:prep_domain(JID), 
           exmpp_jid:prep_resource(JID)},
    Ss = case ejabberd_cluster:get_node({LUser, LServer}) of
	     Node when Node == node() ->
		 mnesia:dirty_index_read(session, USR, #session.usr);
	     Node ->
		 catch rpc:call(Node, mnesia, dirty_index_read,
				[session, USR, #session.usr], 5000)
	 end,
    if is_list(Ss), Ss /= [] ->
=======
    ejabberd_sm:route(jlib:make_jid(<<"">>, <<"">>, <<"">>),
		      jlib:make_jid(User, Server, <<"">>),
                      {broadcast, {exit, <<"User removed">>}}).

get_user_resources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US, #session.us) of
	{'EXIT', _Reason} ->
	    [];
	Ss ->
	    [element(3, S#session.usr) || S <- clean_session_list(Ss)]
    end.

-spec get_user_present_resources(binary(), binary()) -> [tuple()].

get_user_present_resources(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> [];
      Ss ->
	  [{S#session.priority, element(3, S#session.usr)}
	   || S <- clean_session_list(Ss),
	      is_integer(S#session.priority)]
    end.

-spec get_user_ip(binary(), binary(), binary()) -> ip().

get_user_ip(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case mnesia:dirty_index_read(session, USR, #session.usr) of
	[] ->
	    undefined;
	Ss ->
>>>>>>> upstream/master
	    Session = lists:max(Ss),
	    proplists:get_value(ip, Session#session.info);
       true ->
	    undefined
    end.

<<<<<<< HEAD
get_user_info(User, Server, Resource) 
    when is_binary(User), 
         is_binary(Server),
         is_binary(Resource) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    LResource = exmpp_stringprep:resourceprep(Resource),
    USR = {LUser, 
           LServer, 
           LResource},
    Ss = case ejabberd_cluster:get_node({LUser, LServer}) of
	     Node when Node == node() ->
		 mnesia:dirty_index_read(session, USR, #session.usr);
	     Node ->
		 catch rpc:call(Node, mnesia, dirty_index_read,
				[session, USR, #session.usr], 5000)
	 end,
    if is_list(Ss), Ss /= [] ->
=======
-spec get_user_info(binary(), binary(), binary()) -> info() | offline.

get_user_info(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case mnesia:dirty_index_read(session, USR, #session.usr) of
	[] ->
	    offline;
	Ss ->
>>>>>>> upstream/master
	    Session = lists:max(Ss),
	    Conn = proplists:get_value(conn, Session#session.info),
	    IP = proplists:get_value(ip, Session#session.info),
	    Priority = Session#session.priority, %% integer()
	    {CreationNow, Pid} = Session#session.sid,
	    CreationString = jlib:now_to_utc_string(CreationNow),
	    [{node, Node}, {conn, Conn}, {ip, IP}, {priority, Priority}, {pid, Pid}, {creation, CreationString}];
       true ->
	    offline
    end.

<<<<<<< HEAD
set_presence(SID, JID, Priority, Presence, Info) when ?IS_JID(JID) ->
    set_session(SID, JID, Priority, Info),
    ejabberd_hooks:run(set_presence_hook, 
           exmpp_jid:prep_domain(JID),
		       [exmpp_jid:prep_node(JID), 
                exmpp_jid:prep_domain(JID), 
                exmpp_jid:prep_resource(JID), 
                Presence]).

unset_presence(SID, JID, Status, Info) when ?IS_JID(JID)->
    set_session(SID, JID, undefined, Info),
    ejabberd_hooks:run(unset_presence_hook, 
               exmpp_jid:prep_domain(JID),
		       [exmpp_jid:prep_node(JID), 
                exmpp_jid:prep_domain(JID), 
                exmpp_jid:prep_resource(JID), 
                Status]).

close_session_unset_presence(SID, JID, Status) when ?IS_JID(JID) ->
    close_session(SID, JID),
    ejabberd_hooks:run(unset_presence_hook, 
               exmpp_jid:prep_domain(JID),
		       [exmpp_jid:prep_node(JID), 
                exmpp_jid:prep_domain(JID), 
                exmpp_jid:prep_resource(JID), 
                Status]).

get_session_pid(JID) when ?IS_JID(JID) ->
    get_session_pid({exmpp_jid:prep_node(JID), 
		     exmpp_jid:prep_domain(JID), 
		     exmpp_jid:prep_resource(JID)});
get_session_pid({LUser, LServer, _} = USR) ->
    Res = case ejabberd_cluster:get_node({LUser, LServer}) of
	      Node when Node == node() ->
		  mnesia:dirty_index_read(session, USR, #session.usr);
	      Node ->
		  catch rpc:call(Node, mnesia, dirty_index_read,
				 [session, USR, #session.usr], 5000)
	  end,
    case Res of
=======
-spec set_presence(sid(), binary(), binary(), binary(),
                   prio(), xmlel(), info()) -> ok.

set_presence(SID, User, Server, Resource, Priority,
	     Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority,
		Info),
    ejabberd_hooks:run(set_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Presence]).

-spec unset_presence(sid(), binary(), binary(),
                     binary(), binary(), info()) -> ok.

unset_presence(SID, User, Server, Resource, Status,
	       Info) ->
    set_session(SID, User, Server, Resource, undefined,
		Info),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec close_session_unset_presence(sid(), binary(), binary(),
                                   binary(), binary()) -> ok.

close_session_unset_presence(SID, User, Server,
			     Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec get_session_pid(binary(), binary(), binary()) -> none | pid().

get_session_pid(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case catch mnesia:dirty_index_read(session, USR, #session.usr) of
>>>>>>> upstream/master
	[#session{sid = {_, Pid}}] -> Pid;
	_ -> none
    end.

-spec dirty_get_sessions_list() -> [ljid()].

dirty_get_sessions_list() ->
    Match = [{#session{usr = '$1', _ = '_'}, [], ['$1']}],
    lists:flatmap(
      fun(Node) when Node == node() ->
	      mnesia:dirty_select(session, Match);
	 (Node) ->
	      case catch rpc:call(Node, mnesia, dirty_select,
				  [session, Match], 5000) of
		  Ss when is_list(Ss) ->
		      Ss;
		  _ ->
		      []
	      end
      end, ejabberd_cluster:get_nodes()).

dirty_get_my_sessions_list() ->
    mnesia:dirty_match_object(#session{_ = '_'}).

<<<<<<< HEAD
get_vh_my_session_list(Server) when is_binary(Server) ->
    LServer = exmpp_stringprep:nameprep(Server),
=======
-spec get_vh_session_list(binary()) -> [ljid()].

get_vh_session_list(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(session,
			[{#session{usr = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$1']}]).

-spec get_all_pids() -> [pid()].

get_all_pids() ->
>>>>>>> upstream/master
    mnesia:dirty_select(
      session,
      ets:fun2ms(
        fun(#session{sid = {_, Pid}}) ->
		Pid
        end)).

get_vh_session_list(Server) when is_binary(Server) ->
    lists:flatmap(
      fun(Node) when Node == node() ->
	      get_vh_my_session_list(Server);
	 (Node) ->
	      case catch rpc:call(Node, ?MODULE, get_vh_my_session_list,
				  [Server], 5000) of
		  Ss when is_list(Ss) ->
		      Ss;
		  _ ->
		      []
	      end
      end, ejabberd_cluster:get_nodes()).

get_vh_session_number(Server) ->
<<<<<<< HEAD
    %% TODO
    length(get_vh_session_list(Server)).
=======
    LServer = jlib:nameprep(Server),
    Query = mnesia:dirty_select(
		session_counter,
		[{#session_counter{vhost = LServer, count = '$1'},
		  [],
		  ['$1']}]),
    case Query of
	[Count] ->
	    Count;
	_ -> 0
    end.
>>>>>>> upstream/master

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun}.

-spec register_iq_handler(binary(), binary(), atom(), atom(), list()) -> any().

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

-spec unregister_iq_handler(binary(), binary()) -> any().

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.

migrate(After) ->
    Ss = mnesia:dirty_select(
	   session,
	   [{#session{us = '$1', sid = {'_', '$2'}, _ = '_'},
	     [],
	     ['$$']}]),
    lists:foreach(
      fun([US, Pid]) ->
	      case ejabberd_cluster:get_node_new(US) of
		  Node when Node /= node() ->
		      ejabberd_c2s:migrate(Pid, Node, After);
		  _ ->
		      ok
	      end
      end, Ss).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(session,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies),
<<<<<<< HEAD
=======
    mnesia:add_table_copy(session_counter, node(), ram_copies),
    mnesia:subscribe(system),
>>>>>>> upstream/master
    ets:new(sm_iqtable, [named_table]),
    ejabberd_hooks:add(roster_in_subscription, global,
			ejabberd_sm, check_in_subscription, 20),
    ejabberd_hooks:add(offline_message_hook, global,
			ejabberd_sm, bounce_offline_message, 100),
    ejabberd_hooks:add(remove_user, global,
			ejabberd_sm, disconnect_removed_user, 100),
    ejabberd_hooks:add(node_hash_update, ?MODULE, migrate, 100),
    lists:foreach(
      fun(Host) ->
          HostB = list_to_binary(Host),
	      ejabberd_hooks:add(roster_in_subscription, HostB,
				 ejabberd_sm, check_in_subscription, 20),
	      ejabberd_hooks:add(offline_message_hook, HostB,
				 ejabberd_sm, bounce_offline_message, 100),
	      ejabberd_hooks:add(remove_user, HostB,
				 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_commands:register_commands(commands()),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% #xmlelement{} used for retro-compatibility
handle_info({route, FromOld, ToOld, #xmlelement{} = PacketOld}, State) ->
    catch throw(for_stacktrace), % To have a stacktrace.
    io:format("~nSM: old #xmlelement:~n~p~n~p~n~n",
      [PacketOld, erlang:get_stacktrace()]),
    % XXX OLD FORMAT: From, To, Packet.
    From = jlib:from_old_jid(FromOld),
    To = jlib:from_old_jid(ToOld),
    Packet = exmpp_xml:xmlelement_to_xmlel(PacketOld, [?NS_JABBER_CLIENT],
      [{?NS_XMPP, ?NS_XMPP_pfx}]),
    handle_info({route, From, To, Packet}, State);
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
<<<<<<< HEAD
=======
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    recount_session_table(Node),
    {noreply, State};
>>>>>>> upstream/master
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, ejabberd:normalize_host(Host)}, Module, Function}),
    {noreply, State};
<<<<<<< HEAD
handle_info({register_iq_handler, Host, XMLNS, Module, Function, Opts}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, ejabberd:normalize_host(Host)}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(sm_iqtable, {XMLNS, ejabberd:normalize_host(Host)}) of
	[{_, Module, Function, Opts}] ->
	    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
	_ ->
	    ok
=======
handle_info({register_iq_handler, Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(sm_iqtable,
	       {{XMLNS, Host}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
>>>>>>> upstream/master
    end,
    ets:delete(sm_iqtable, {XMLNS, ejabberd:normalize_host(Host)}),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ejabberd_hooks:delete(node_hash_update, ?MODULE, migrate, 100),
    ejabberd_commands:unregister_commands(commands()),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

<<<<<<< HEAD
set_session(SID, JID, Priority, Info) ->
    US = {exmpp_jid:prep_node(JID), exmpp_jid:prep_domain(JID)},
    USR = {exmpp_jid:prep_node(JID), 
           exmpp_jid:prep_domain(JID), 
           exmpp_jid:prep_resource(JID)},
    F = fun() ->
		mnesia:write(#session{sid = SID,
				      usr = USR,
				      us = US,
				      priority = Priority,
				      info = Info})
	end,
    mnesia:sync_dirty(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    {U, S} = {exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To)},
    case ejabberd_cluster:get_node({U, S}) of
	Node when Node /= node() ->
	    {?MODULE, Node} ! {route, From, To, Packet};
	_ ->
	    do_route1(From, To, Packet)
    end.

do_route1(From, To, Packet) ->
    case exmpp_jid:prep_resource(To) of
	undefined ->
	    case Packet of
		_ when ?IS_PRESENCE(Packet) ->
		    {Pass, _Subsc} =
			case exmpp_presence:get_type(Packet) of
			    'subscribe' ->
				Reason = exmpp_presence:get_status(Packet),
				{is_privacy_allow(From, To, Packet) andalso
				 ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   exmpp_jid:prep_domain(To),
				   false,
				   [exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To), From, subscribe, Reason]),
				 true};
			    'subscribed' ->
				{is_privacy_allow(From, To, Packet) andalso
				 ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   exmpp_jid:prep_domain(To),
				   false,
				   [exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To), From, subscribed, <<>>]),
				 true};
			    'unsubscribe' ->
				{is_privacy_allow(From, To, Packet) andalso
				 ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   exmpp_jid:prep_domain(To),
				   false,
				   [exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To), From, unsubscribe, <<>>]),
				 true};
			    'unsubscribed' ->
				{is_privacy_allow(From, To, Packet) andalso
				 ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   exmpp_jid:prep_domain(To),
				   false,
				   [exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To), From, unsubscribed, <<>>]),
				 true};
			    _ ->
				{true, false}
			end,
		    if Pass ->
			    PResources = get_user_present_resources(
					   exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To)),
			    lists:foreach(
			      fun({_, R}) ->
				      do_route(
					From,
					exmpp_jid:full(To, R),
					Packet)
			      end, PResources);
		       true ->
			    ok
		    end;
		_ when ?IS_MESSAGE(Packet) ->
		    route_message(From, To, Packet);
		_ when ?IS_IQ(Packet) ->
		    process_iq(From, To, Packet);
		#xmlel{name = 'broadcast'} ->
		    lists:foreach(
		      fun(R) ->
			      do_route(From,
				       exmpp_jid:full(To, R),
				       Packet)
		      end, get_user_resources(exmpp_jid:prep_node(To), 
                                      exmpp_jid:prep_domain(To)));
		_ ->
		    ok
	    end;
	_ ->
	    USR = {exmpp_jid:prep_node(To),
               exmpp_jid:prep_domain(To),
               exmpp_jid:prep_resource(To)},
	    case mnesia:dirty_index_read(session, USR, #session.usr) of
		[] ->
		    case Packet of
			_ when ?IS_MESSAGE(Packet) ->
			    route_message(From, To, Packet);
			_ when ?IS_IQ(Packet) ->
			    case exmpp_iq:get_type(Packet) of
				'error' -> ok;
				'result' -> ok;
				_ ->
				    Err =
					exmpp_iq:error(Packet,
                                          'service-unavailable'),
				    ejabberd_router:route(To, From, Err)
			    end;
=======
set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    F = fun () ->
		mnesia:write(#session{sid = SID, usr = USR, us = US,
				      priority = Priority, info = Info})
	end,
    mnesia:sync_dirty(F).

%% Recalculates alive sessions when Node goes down 
%% and updates session and session_counter tables 
recount_session_table(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       session,
		       [{#session{sid = {'_', '$1'}, _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete({session, E#session.sid})
			      end, Es),
		%% reset session_counter table with active sessions
		mnesia:clear_table(session_counter),
		lists:foreach(fun(Server) ->
				LServer = jlib:nameprep(Server),
				Hs = mnesia:select(session,
				    [{#session{usr = '$1', _ = '_'},
				    [{'==', {element, 2, '$1'}, LServer}],
				    ['$1']}]),
				mnesia:write(
				    #session_counter{vhost = LServer, 
						     count = length(Hs)})
			      end, ?MYHOSTS)
	end,
    mnesia:async_dirty(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, {broadcast, _} = Packet) ->
    case To#jid.lresource of
        <<"">> ->
            lists:foreach(fun(R) ->
                                  do_route(From,
                                           jlib:jid_replace_resource(To, R),
                                           Packet)
                          end,
                          get_user_resources(To#jid.user, To#jid.server));
        _ ->
            USR = jlib:jid_tolower(To),
            case mnesia:dirty_index_read(session, USR, #session.usr) of
                [] ->
                    ?DEBUG("packet dropped~n", []);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, Packet}
            end
    end;
do_route(From, To, #xmlel{} = Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket "
	   "~P~n",
	   [From, To, Packet, 8]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer, lresource = LResource} = To,
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case LResource of
      <<"">> ->
	  case Name of
	    <<"presence">> ->
		{Pass, _Subsc} = case xml:get_attr_s(<<"type">>, Attrs)
				     of
				   <<"subscribe">> ->
				       Reason = xml:get_path_s(Packet,
							       [{elem,
								 <<"status">>},
								cdata]),
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribe,
								   Reason]),
					true};
				   <<"subscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribed,
								   <<"">>]),
					true};
				   <<"unsubscribe">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribe,
								   <<"">>]),
					true};
				   <<"unsubscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribed,
								   <<"">>]),
					true};
				   _ -> {true, false}
				 end,
		if Pass ->
		       PResources = get_user_present_resources(LUser, LServer),
		       lists:foreach(fun ({_, R}) ->
					     do_route(From,
						      jlib:jid_replace_resource(To,
										R),
						      Packet)
				     end,
				     PResources);
		   true -> ok
		end;
	    <<"message">> -> route_message(From, To, Packet);
	    <<"iq">> -> process_iq(From, To, Packet);
	    _ -> ok
	  end;
      _ ->
	  USR = {LUser, LServer, LResource},
	  case mnesia:dirty_index_read(session, USR, #session.usr)
	      of
	    [] ->
		case Name of
		  <<"message">> -> route_message(From, To, Packet);
		  <<"iq">> ->
		      case xml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> -> ok;
			<<"result">> -> ok;
>>>>>>> upstream/master
			_ ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		      end;
		  _ -> ?DEBUG("packet droped~n", [])
		end;
	    Ss ->
		Session = lists:max(Ss),
		Pid = element(2, Session#session.sid),
		?DEBUG("sending to process ~p~n", [Pid]),
		Pid ! {route, From, To, Packet}
	  end
    end.

%% The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
is_privacy_allow(From, To, Packet) ->
<<<<<<< HEAD
    User = exmpp_jid:prep_node(To), 
    Server = exmpp_jid:prep_domain(To),
    PrivacyList = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
					  #userlist{}, [User, Server]),
=======
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList =
	ejabberd_hooks:run_fold(privacy_get_user_list, Server,
				#userlist{}, [User, Server]),
>>>>>>> upstream/master
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
is_privacy_allow(From, To, Packet, PrivacyList) ->
<<<<<<< HEAD
    User = exmpp_jid:prep_node(To), 
    Server = exmpp_jid:prep_domain(To),
    allow == ejabberd_hooks:run_fold(
	       privacy_check_packet, Server,
	       allow,
	       [User,
		Server,
		PrivacyList,
		{From, To, Packet},
		in]).
=======
    User = To#jid.user,
    Server = To#jid.server,
    allow ==
      ejabberd_hooks:run_fold(privacy_check_packet, Server,
			      allow,
			      [User, Server, PrivacyList, {From, To, Packet},
			       in]).
>>>>>>> upstream/master

route_message(From, To, Packet) ->
    LUser = exmpp_jid:prep_node(To),
    LServer = exmpp_jid:prep_domain(To),
    PrioRes = get_user_present_resources(LUser, LServer),
    case catch lists:max(PrioRes) of
<<<<<<< HEAD
	{Priority, _R} when is_integer(Priority), Priority >= 0 ->
	    lists:foreach(
	      %% Route messages to all priority that equals the max, if
	      %% positive
	      fun({P, R}) when P == Priority ->
		      USR = {LUser, LServer, R},
		      case mnesia:dirty_index_read(session, USR, #session.usr) of
			  [] ->
			      ok; % Race condition
			  Ss ->
			      Session = lists:max(Ss),
			      Pid = element(2, Session#session.sid),
			      ?DEBUG("sending to process ~p~n", [Pid]),
			      Pid ! {route, From, To, Packet}
		      end;
		 %% Ignore other priority:
		 ({_Prio, _Res}) ->
		      ok
	      end,
	      PrioRes);
	_ ->
	    case exmpp_message:get_type(Packet) of
		'error' ->
		    ok;
		'groupchat' ->
		    bounce_offline_message(From, To, Packet);
		'headline' ->
		    bounce_offline_message(From, To, Packet);
		_ ->
		    case ejabberd_auth:is_user_exists(exmpp_jid:prep_node_as_list(To), 
                                              exmpp_jid:prep_domain_as_list(To)) of
			true ->
			    case is_privacy_allow(From, To, Packet) of
				true ->
				    ejabberd_hooks:run(offline_message_hook,
						       exmpp_jid:prep_domain(To),
						       [From, To, Packet]);
				false ->
				    ok
			    end;
			_ ->
			    Err = exmpp_stanza:reply_with_error(
				    Packet, 'service-unaivailable'),
			    ejabberd_router:route(To, From, Err)
		    end
	    end
=======
      {Priority, _R}
	  when is_integer(Priority), Priority >= 0 ->
	  lists:foreach(fun ({P, R}) when P == Priority ->
				LResource = jlib:resourceprep(R),
				USR = {LUser, LServer, LResource},
				case mnesia:dirty_index_read(session, USR,
							     #session.usr)
				    of
				  [] ->
				      ok; % Race condition
				  Ss ->
				      Session = lists:max(Ss),
				      Pid = element(2, Session#session.sid),
				      ?DEBUG("sending to process ~p~n", [Pid]),
				      Pid ! {route, From, To, Packet}
				end;
			    %% Ignore other priority:
			    ({_Prio, _Res}) -> ok
			end,
			PrioRes);
      _ ->
	  case xml:get_tag_attr_s(<<"type">>, Packet) of
	    <<"error">> -> ok;
	    <<"groupchat">> ->
		bounce_offline_message(From, To, Packet);
	    <<"headline">> ->
		bounce_offline_message(From, To, Packet);
	    _ ->
		case ejabberd_auth:is_user_exists(LUser, LServer) of
		  true ->
		      case is_privacy_allow(From, To, Packet) of
			true ->
			    ejabberd_hooks:run(offline_message_hook, LServer,
					       [From, To, Packet]);
			false -> ok
		      end;
		  _ ->
		      Err = jlib:make_error_reply(Packet,
						  ?ERR_SERVICE_UNAVAILABLE),
		      ejabberd_router:route(To, From, Err)
		end
	  end
>>>>>>> upstream/master
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) -> Res;
clean_session_list([S], Res) -> [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if S1#session.usr == S2#session.usr ->
	   if S1#session.sid > S2#session.sid ->
		  clean_session_list([S1 | Rest], Res);
	      true -> clean_session_list([S2 | Rest], Res)
	   end;
       true -> clean_session_list([S2 | Rest], [S1 | Res])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% On new session, check if some existing connections need to be replace
<<<<<<< HEAD
check_for_sessions_to_replace(JID) ->
    %% TODO: Depending on how this is executed, there could be an unneeded
    %% replacement for max_sessions. We need to check this at some point.
    check_existing_resources(JID),
    check_max_sessions(JID).

check_existing_resources(JID) ->
    %% A connection exist with the same resource. We replace it:
    SIDs = get_resource_sessions(JID),
    if
	SIDs == [] -> ok;
	true ->
	    %% A connection exist with the same resource. We replace it:
	    MaxSID = lists:max(SIDs),
	    lists:foreach(
	      fun({_, Pid} = S) when S /= MaxSID ->
		      Pid ! replaced;
		 (_) -> ok
	      end, SIDs)
    end.

is_existing_resource(U, S, R) ->
    [] /= get_resource_sessions(exmpp_jid:make(U, S, R)).

get_resource_sessions(JID) ->
    USR = {exmpp_jid:prep_node(JID),
           exmpp_jid:prep_domain(JID),
           exmpp_jid:prep_resource(JID)},
    mnesia:dirty_select(
	     session,
	     [{#session{sid = '$1', usr = USR, _ = '_'}, [], ['$1']}]).

check_max_sessions(JID) ->
    %% If the max number of sessions for a given is reached, we replace the
    %% first one
    SIDs = mnesia:dirty_select(
	     session,
	     [{#session{sid = '$1', 
                    us = {exmpp_jid:prep_node(JID), exmpp_jid:prep_domain(JID)},
                    _ = '_'}, [],
	       ['$1']}]),
    MaxSessions = get_max_user_sessions(JID),
    if
	length(SIDs) =< MaxSessions ->
	    ok;
	true ->
	    {_, Pid} = lists:min(SIDs),
	    Pid ! replaced
=======
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

check_existing_resources(LUser, LServer, LResource) ->
    SIDs = get_resource_sessions(LUser, LServer, LResource),
    if SIDs == [] -> ok;
       true ->
	   MaxSID = lists:max(SIDs),
	   lists:foreach(fun ({_, Pid} = S) when S /= MaxSID ->
				 Pid ! replaced;
			     (_) -> ok
			 end,
			 SIDs)
    end.

-spec is_existing_resource(binary(), binary(), binary()) -> boolean().

is_existing_resource(LUser, LServer, LResource) ->
    [] /= get_resource_sessions(LUser, LServer, LResource).

get_resource_sessions(User, Server, Resource) ->
    USR = {jlib:nodeprep(User), jlib:nameprep(Server),
	   jlib:resourceprep(Resource)},
    mnesia:dirty_select(session,
			[{#session{sid = '$1', usr = USR, _ = '_'}, [],
			  ['$1']}]).

check_max_sessions(LUser, LServer) ->
    SIDs = mnesia:dirty_select(session,
			       [{#session{sid = '$1', us = {LUser, LServer},
					  _ = '_'},
				 [], ['$1']}]),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(SIDs) =< MaxSessions -> ok;
       true -> {_, Pid} = lists:min(SIDs), Pid ! replaced
>>>>>>> upstream/master
    end.

%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
<<<<<<< HEAD
get_max_user_sessions(JID) ->
    case acl:match_rule(
	   exmpp_jid:prep_domain_as_list(JID), max_user_sessions, exmpp_jid:bare(JID)) of
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_SESSIONS
=======
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(Host, max_user_sessions,
			jlib:make_jid(LUser, Host, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      infinity -> infinity;
      _ -> ?MAX_USER_SESSIONS
>>>>>>> upstream/master
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
<<<<<<< HEAD
    case exmpp_iq:xmlel_to_iq(Packet) of
	#iq{kind = request, ns = XMLNS} = IQ_Rec ->
        LServer = exmpp_jid:prep_domain(To),
	    case ets:lookup(sm_iqtable, {XMLNS, ejabberd:normalize_host(LServer)}) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ_Rec),
		    if
			ResIQ /= ignore ->
			    Reply = exmpp_iq:iq_to_xmlel(ResIQ, To, From),
			    ejabberd_router:route(To, From, Reply);
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:handle(LServer,
			Module, Function, Opts, From, To, IQ_Rec);
		[] ->
		    case ets:lookup(sm_iqtable, {XMLNS, global}) of
			[{_, Module, Function, Opts}] ->
			    gen_iq_handler:handle(global, Module, Function, Opts,
						  From, To, IQ_Rec);
			[] ->
			    Err = exmpp_iq:error(Packet, 'service-unavailable'),
			    ejabberd_router:route(To, From, Err)
		    end
	    end;
	#iq{kind = response} ->
	    ok;
	_ ->
	    Err = exmpp_iq:error(Packet, 'bad-request'),
	    ejabberd_router:route(To, From, Err),
	    ok
    end.

force_update_presence({LUser, _LServer} = US) ->
    case catch mnesia:dirty_index_read(session, US, #session.us) of
        {'EXIT', _Reason} ->
            ok;
        Ss ->
            lists:foreach(fun(#session{sid = {_, Pid}}) ->
                                  Pid ! {force_update_presence, LUser}
                          end, Ss)
    end.

=======
    IQ = jlib:iq_query_info(Packet),
    case IQ of
      #iq{xmlns = XMLNS} ->
	  Host = To#jid.lserver,
	  case ets:lookup(sm_iqtable, {XMLNS, Host}) of
	    [{_, Module, Function}] ->
		ResIQ = Module:Function(From, To, IQ),
		if ResIQ /= ignore ->
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
		   true -> ok
		end;
	    [{_, Module, Function, Opts}] ->
		gen_iq_handler:handle(Host, Module, Function, Opts,
				      From, To, IQ);
	    [] ->
		Err = jlib:make_error_reply(Packet,
					    ?ERR_SERVICE_UNAVAILABLE),
		ejabberd_router:route(To, From, Err)
	  end;
      reply -> ok;
      _ ->
	  Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	  ejabberd_router:route(To, From, Err),
	  ok
    end.

-spec force_update_presence({binary(), binary()}) -> any().

force_update_presence({LUser, _LServer} = US) ->
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> ok;
      Ss ->
	  lists:foreach(fun (#session{sid = {_, Pid}}) ->
				Pid ! {force_update_presence, LUser}
			end,
			Ss)
    end.
>>>>>>> upstream/master

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

commands() ->
    [#ejabberd_commands{name = connected_users,
			tags = [session],
			desc = "List all established sessions",
			module = ?MODULE, function = connected_users, args = [],
			result = {connected_users, {list, {sessions, string}}}},
     #ejabberd_commands{name = connected_users_number,
			tags = [session, stats],
			desc = "Get the number of established sessions",
			module = ?MODULE, function = connected_users_number,
			args = [], result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
			tags = [session],
			desc = "List user's connected resources",
			module = ?MODULE, function = user_resources,
			args = [{user, binary}, {host, binary}],
			result = {resources, {list, {resource, string}}}},
     #ejabberd_commands{name = kick_user,
			tags = [session],
			desc = "Disconnect user's active sessions",
			module = ?MODULE, function = kick_user,
			args = [{user, binary}, {host, binary}],
			result = {num_resources, integer}}].

-spec connected_users() -> [binary()].

connected_users() ->
    USRs = dirty_get_sessions_list(),
    SUSRs = lists:sort(USRs),
    lists:map(fun ({U, S, R}) -> <<U/binary, $@, S/binary, $/, R/binary>> end,
	      SUSRs).

connected_users_number() ->
    length(dirty_get_sessions_list()).

user_resources(User, Server) ->
<<<<<<< HEAD
    Resources =  get_user_resources(list_to_binary(User), 
                                    list_to_binary(Server)),
=======
    Resources = get_user_resources(User, Server),
>>>>>>> upstream/master
    lists:sort(Resources).

kick_user(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:foreach(
	fun(Resource) ->
		PID = get_session_pid(User, Server, Resource),
		PID ! disconnect
	end, Resources),
    length(Resources).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update Mnesia tables

update_tables() ->
    case catch mnesia:table_info(session, attributes) of
      [ur, user, node] -> mnesia:delete_table(session);
      [ur, user, pid] -> mnesia:delete_table(session);
      [usr, us, pid] -> mnesia:delete_table(session);
      [usr, us, sid, priority, info] -> mnesia:delete_table(session);
      [sid, usr, us, priority] ->
	  mnesia:delete_table(session);
      [sid, usr, us, priority, info] -> ok;
      {'EXIT', _} -> ok
    end,
    case lists:member(presence, mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(presence);
      false -> ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end,
    mnesia:delete_table(session_counter),
    case catch mnesia:table_info(session, local_content) of
	false ->
	    mnesia:delete_table(session);
	_ ->
	    ok
    end.
