%%%----------------------------------------------------------------------
%%% File    : mod_caps.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Request and cache Entity Capabilities (XEP-0115)
%%% Created : 7 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
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
%%% 2009, improvements from ProcessOne to support correct PEP handling
%%% through s2s, use less memory, and speedup global caps handling
%%%----------------------------------------------------------------------

-module(mod_caps).

-author('henoch@dtek.chalmers.se').

-behaviour(gen_server).

-behaviour(gen_mod).

<<<<<<< HEAD
-export([read_caps/1,
	 caps_stream_features/2,
	 disco_features/5,
	 disco_identity/5,
	 disco_info/5,
=======
-export([read_caps/1, caps_stream_features/2,
	 disco_features/5, disco_identity/5, disco_info/5,
>>>>>>> upstream/master
	 get_features/1]).

%% gen_mod callbacks
-export([start/2, start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

%% hook handlers
-export([user_send_packet/3,
	 user_receive_packet/4,
	 c2s_presence_in/2,
	 c2s_broadcast_recipients/5]).

<<<<<<< HEAD

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-include("ejabberd.hrl").

-define(PROCNAME, ejabberd_mod_caps).
-define(BAD_HASH_LIFETIME, 600). %% in seconds

%% TODO : -type and -spec

-record(caps,
{
    node    :: binary(),
    version :: binary(),
    hash    :: binary(),
    exts    :: [] | [binary(),...]
}).

-record(caps_features,
{
    node_pair     :: binary(),
    features = [] :: [] | [binary(),...]
}).

-record(state, {host}).
=======
-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_caps).

-define(BAD_HASH_LIFETIME, 600).

-record(caps,
{
    node    = <<"">> :: binary(),
    version = <<"">> :: binary(),
    hash    = <<"">> :: binary(),
    exts    = []     :: [binary()]
}).

-type caps() :: #caps{}.

-export_type([caps/0]).

-record(caps_features,
{
    node_pair = {<<"">>, <<"">>} :: {binary(), binary()},
    features  = []               :: [binary()] | pos_integer()
}).

-record(state, {host = <<"">> :: binary()}).
>>>>>>> upstream/master

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

<<<<<<< HEAD
start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    Proc = gen_mod:get_module_proc(HostB, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [HostB, Opts]},
	 transient,
	 1000,
	 worker,
	 [?MODULE]},
=======
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
>>>>>>> upstream/master
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).
<<<<<<< HEAD

%% get_features returns a list of features implied by the given caps
%% record (as extracted by read_caps) or 'unknown' if features are
%% not completely collected at the moment.
get_features(nothing) ->
    [];
get_features(#caps{node = Node, version = Version, exts = Exts}) ->
    SubNodes = [Version | Exts],
    lists:foldl(
      fun(SubNode, Acc) ->
	      case cache_tab:lookup(caps_features, {Node, SubNode},
				    caps_read_fun({Node, SubNode})) of
		  error ->
			Acc;
		  {ok, Features} when is_list(Features) ->
			Features ++ Acc;
		  _ ->
			Acc
	      end
      end, [], SubNodes).

%% read_caps takes a list of XML elements (the child elements of a
%% <presence/> stanza) and returns an opaque value representing the
%% Entity Capabilities contained therein, or the atom nothing if no
%% capabilities are advertised.
read_caps(Els) ->
    read_caps(Els, nothing).

read_caps([#xmlel{ns = ?NS_CAPS, name = 'c'} = El | Tail], _Result) ->
    Node = exmpp_xml:get_attribute(El, <<"node">>, <<>>),
    Version = exmpp_xml:get_attribute(El, <<"ver">>, <<>>),
    Hash = exmpp_xml:get_attribute(El, <<"hash">>, <<>>),
    Exts = [list_to_binary(Feature)
	    || Feature <- string:tokens(exmpp_xml:get_attribute_as_list(El, <<"ext">>, ""), " ")],
    read_caps(Tail, #caps{node = Node, hash = Hash, version = Version, exts = Exts});
read_caps([#xmlel{ns = ?NS_MUC_USER, name = 'x'} | _Tail], _Result) ->
    nothing;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) ->
    Result.

%%====================================================================
%% Hooks
%%====================================================================
user_send_packet(#jid{node = U, domain = S} = From, #jid{node = U, domain = S} = _To,
		 #xmlel{name = 'presence', attrs = Attrs, children = Els}) ->
    Type = exmpp_xml:get_attribute_from_list(Attrs, <<"type">>, <<>>),
    if Type == <<>> ; Type == <<"available">> ->
	    case read_caps(exmpp_xml:remove_cdata_from_list(Els)) of
		nothing ->
		    ok;
		#caps{version = Version, exts = Exts} = Caps ->
		    feature_request(_Server = binary_to_list(S), From, Caps, [Version | Exts])
		    end;
	true ->
	    ok
    end;
user_send_packet(_From, _To, _Packet) ->
    ok.

user_receive_packet(#jid{domain = Server}, From, _To,
		    #xmlel{name = 'presence', attrs = Attrs, children = Els}) ->
    Type = exmpp_xml:get_attribute_from_list(Attrs, <<"type">>, <<>>),
    if Type == <<>> ; Type == <<"available">> ->
	    case read_caps(exmpp_xml:remove_cdata_from_list(Els)) of
		nothing ->
		    ok;
		#caps{version = Version, exts = Exts} = Caps ->
		    feature_request(Server, From, Caps, [Version | Exts])
	    end;
	true -> ok
    end;
user_receive_packet(_JID, _From, _To, _Packet) ->
    ok.

caps_stream_features(Acc, MyHost) ->
    case make_my_disco_hash(MyHost) of
	<<"">> ->
	    Acc;
	Hash ->
	    [#xmlel{name = c,
		    ns = ?NS_CAPS,
		    attrs = [?XMLATTR(<<"hash">>, "sha-1"),
			     ?XMLATTR(<<"node">>, ?EJABBERD_URI),
			     ?XMLATTR(<<"ver">>, Hash)]} | Acc]
    end.

disco_features(_Acc, From, To, <<?EJABBERD_URI, $#, _/binary>>, Lang) ->
    ejabberd_hooks:run_fold(disco_local_features,
			    To#jid.domain,
			    empty,
			    [From, To, <<>>, Lang]);
disco_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_identity(_Acc, From, To, <<?EJABBERD_URI, $#, _/binary>>, Lang) ->
    ejabberd_hooks:run_fold(disco_local_identity,
			    To#jid.domain,
			    [],
			    [From, To, <<>>, Lang]);
disco_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_info(_Acc, Host, Module, <<?EJABBERD_URI, $#, _/binary>>, Lang) ->
    ejabberd_hooks:run_fold(disco_info,
			    list_to_binary(Host),
			    [],
			    [Host, Module, <<>>, Lang]);
disco_info(Acc, _Host, _Module, _Node, _Lang) ->
    Acc.

c2s_presence_in(C2SState, {From, To, #xmlel{attrs = Attrs, children = Els}}) ->
    Type = exmpp_xml:get_attribute_from_list(Attrs, <<"type">>, <<>>),
    Subscription = ejabberd_c2s:get_subscription(From, C2SState),
    Insert = ((Type == <<>>) or (Type == <<"available">>))
	and ((Subscription == both) or (Subscription == to)),
    Delete = (Type == <<"unavailable">>) or (Type == <<"error">>),
    if Insert or Delete ->
	    LFrom = exmpp_jid:to_lower(From),
	    Rs = case ejabberd_c2s:get_aux_field(caps_resources, C2SState) of
		    {ok, Rs1} ->
			Rs1;
		    error ->
			gb_trees:empty()
	    end,
	    Caps = read_caps(Els),
	    {CapsUpdated, NewRs} =
		case Caps of
		    nothing when Insert == true ->
			{false, Rs};
		    _ when Insert == true ->
			case gb_trees:lookup(LFrom, Rs) of
			    {value, Caps} ->
				{false, Rs};
			    none ->
				{true, gb_trees:insert(LFrom, Caps, Rs)};
			    _ ->
				{true, gb_trees:update(LFrom, Caps, Rs)}
			end;
		    _ ->
			{false, gb_trees:delete_any(LFrom, Rs)}
		end,
	    if CapsUpdated ->
		    ejabberd_hooks:run(caps_update, To#jid.domain,
				       [From, To, get_features(Caps)]);
		true ->
		    ok
	    end,
	    ejabberd_c2s:set_aux_field(caps_resources, NewRs, C2SState);
	true ->
	    C2SState
    end.

=======

%% get_features returns a list of features implied by the given caps
%% record (as extracted by read_caps) or 'unknown' if features are
%% not completely collected at the moment.
get_features(nothing) -> [];
get_features(#caps{node = Node, version = Version, exts = Exts}) ->
    SubNodes = [Version | Exts],
%% read_caps takes a list of XML elements (the child elements of a
%% <presence/> stanza) and returns an opaque value representing the
%% Entity Capabilities contained therein, or the atom nothing if no
%% capabilities are advertised.
    lists:foldl(fun (SubNode, Acc) ->
			NodePair = {Node, SubNode},
			case cache_tab:lookup(caps_features, NodePair,
					      caps_read_fun(NodePair))
			    of
			  {ok, Features} when is_list(Features) ->
			      Features ++ Acc;
			  _ -> Acc
			end
		end,
		[], SubNodes).

read_caps(Els) -> read_caps(Els, nothing).

read_caps([#xmlel{name = <<"c">>, attrs = Attrs}
	   | Tail],
	  Result) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
      ?NS_CAPS ->
	  Node = xml:get_attr_s(<<"node">>, Attrs),
	  Version = xml:get_attr_s(<<"ver">>, Attrs),
	  Hash = xml:get_attr_s(<<"hash">>, Attrs),
	  Exts = str:tokens(xml:get_attr_s(<<"ext">>, Attrs),
			    <<" ">>),
	  read_caps(Tail,
		    #caps{node = Node, hash = Hash, version = Version,
			  exts = Exts});
      _ -> read_caps(Tail, Result)
    end;
read_caps([#xmlel{name = <<"x">>, attrs = Attrs}
	   | Tail],
	  Result) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
      ?NS_MUC_USER -> nothing;
      _ -> read_caps(Tail, Result)
    end;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) -> Result.

%%====================================================================
%% Hooks
%%====================================================================
user_send_packet(
		 #jid{luser = User, lserver = Server} = From,
		 #jid{luser = User, lserver = Server, lresource = <<"">>},
		 #xmlel{name = <<"presence">>, attrs = Attrs, children = Els}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    if Type == <<"">>; Type == <<"available">> ->
	   case read_caps(Els) of
	     nothing -> ok;
	     #caps{version = Version, exts = Exts} = Caps ->
		 feature_request(Server, From, Caps, [Version | Exts])
	   end;
       true -> ok
    end;
user_send_packet(_From, _To, _Packet) -> ok.

user_receive_packet(#jid{lserver = Server}, From, _To,
		    #xmlel{name = <<"presence">>, attrs = Attrs,
			   children = Els}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    IsRemote = not lists:member(From#jid.lserver, ?MYHOSTS),
    if IsRemote and
	 ((Type == <<"">>) or (Type == <<"available">>)) ->
	   case read_caps(Els) of
	     nothing -> ok;
	     #caps{version = Version, exts = Exts} = Caps ->
		 feature_request(Server, From, Caps, [Version | Exts])
	   end;
       true -> ok
    end;
user_receive_packet(_JID, _From, _To, _Packet) ->
    ok.

caps_stream_features(Acc, MyHost) ->
    case make_my_disco_hash(MyHost) of
      <<"">> -> Acc;
      Hash ->
	  [#xmlel{name = <<"c">>,
		  attrs =
		      [{<<"xmlns">>, ?NS_CAPS}, {<<"hash">>, <<"sha-1">>},
		       {<<"node">>, ?EJABBERD_URI}, {<<"ver">>, Hash}],
		  children = []}
	   | Acc]
    end.

disco_features(Acc, From, To, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_local_features,
                                    To#jid.lserver, empty,
                                    [From, To, <<"">>, Lang]);
        false ->
            Acc
    end.

disco_identity(Acc, From, To, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_local_identity,
                                    To#jid.lserver, [],
                                    [From, To, <<"">>, Lang]);
        false ->
            Acc
    end.

disco_info(Acc, Host, Module, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_info, Host, [],
                                    [Host, Module, <<"">>, Lang]);
        false ->
            Acc
    end.

c2s_presence_in(C2SState,
		{From, To, {_, _, Attrs, Els}}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    Subscription = ejabberd_c2s:get_subscription(From,
						 C2SState),
    Insert = ((Type == <<"">>) or (Type == <<"available">>))
	       and ((Subscription == both) or (Subscription == to)),
    Delete = (Type == <<"unavailable">>) or
	       (Type == <<"error">>),
    if Insert or Delete ->
	   LFrom = jlib:jid_tolower(From),
	   Rs = case ejabberd_c2s:get_aux_field(caps_resources,
						C2SState)
		    of
		  {ok, Rs1} -> Rs1;
		  error -> gb_trees:empty()
		end,
	   Caps = read_caps(Els),
	   {CapsUpdated, NewRs} = case Caps of
				    nothing when Insert == true -> {false, Rs};
				    _ when Insert == true ->
					case gb_trees:lookup(LFrom, Rs) of
					  {value, Caps} -> {false, Rs};
					  none ->
					      {true,
					       gb_trees:insert(LFrom, Caps,
							       Rs)};
					  _ ->
					      {true,
					       gb_trees:update(LFrom, Caps, Rs)}
					end;
				    _ -> {false, gb_trees:delete_any(LFrom, Rs)}
				  end,
	   if CapsUpdated ->
		  ejabberd_hooks:run(caps_update, To#jid.lserver,
				     [From, To, get_features(Caps)]);
	      true -> ok
	   end,
	   ejabberd_c2s:set_aux_field(caps_resources, NewRs,
				      C2SState);
       true -> C2SState
    end.

>>>>>>> upstream/master
c2s_broadcast_recipients(InAcc, C2SState, {pep_message, Feature},
			 _From, _Packet) ->
    case ejabberd_c2s:get_aux_field(caps_resources, C2SState) of
	{ok, Rs} ->
	    gb_trees_fold(
<<<<<<< HEAD
		fun(USR, Caps, Acc) ->
		    case lists:member(Feature, get_features(Caps)) of
			true ->
			    [USR|Acc];
			false ->
			    Acc
		    end
	    end, InAcc, Rs);
=======
	      fun(USR, Caps, Acc) ->
		      case lists:member(Feature, get_features(Caps)) of
			  true ->
			      [USR|Acc];
			  false ->
			      Acc
		      end
	      end, InAcc, Rs);
>>>>>>> upstream/master
	_ ->
	    InAcc
    end;
c2s_broadcast_recipients(Acc, _, _, _, _) ->
    Acc.

%%====================================================================
%% gen_server callbacks
%%====================================================================
<<<<<<< HEAD
init([HostB, Opts]) ->
=======
init([Host, Opts]) ->
>>>>>>> upstream/master
    case catch mnesia:table_info(caps_features, storage_type) of
	{'EXIT', _} ->
	    ok;
	disc_only_copies ->
	    ok;
	_ ->
	    mnesia:delete_table(caps_features)
    end,
    mnesia:create_table(caps_features,
			[{disc_only_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, caps_features)}]),
    mnesia:add_table_copy(caps_features, node(), disc_only_copies),
<<<<<<< HEAD
    MaxSize = gen_mod:get_opt(cache_size, Opts, 1000),
    LifeTime = gen_mod:get_opt(cache_life_time, Opts, timer:hours(24) div 1000),
    cache_tab:new(caps_features, [{max_size, MaxSize}, {life_time, LifeTime}]),
    ejabberd_hooks:add(c2s_presence_in, HostB,
		       ?MODULE, c2s_presence_in, 75),
    ejabberd_hooks:add(c2s_broadcast_recipients, HostB,
		       ?MODULE, c2s_broadcast_recipients, 75),
    ejabberd_hooks:add(user_send_packet, HostB,
		       ?MODULE, user_send_packet, 75),
    ejabberd_hooks:add(user_receive_packet, HostB,
		       ?MODULE, user_receive_packet, 75),
    ejabberd_hooks:add(c2s_stream_features, HostB,
		       ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:add(s2s_stream_features, HostB,
		       ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:add(disco_local_features, HostB,
		       ?MODULE, disco_features, 75),
    ejabberd_hooks:add(disco_local_identity, HostB,
		       ?MODULE, disco_identity, 75),
    ejabberd_hooks:add(disco_info, HostB,
		       ?MODULE, disco_info, 75),
    {ok, #state{host = HostB}}.
=======
    MaxSize = gen_mod:get_opt(cache_size, Opts, fun(CS) when is_integer(CS) -> CS end, 1000),
    LifeTime = gen_mod:get_opt(cache_life_time, Opts, fun(CL) when is_integer(CL) -> CL end, timer:hours(24) div 1000),
    cache_tab:new(caps_features, [{max_size, MaxSize}, {life_time, LifeTime}]),
    ejabberd_hooks:add(c2s_presence_in, Host,
		       ?MODULE, c2s_presence_in, 75),
    ejabberd_hooks:add(c2s_broadcast_recipients, Host,
		       ?MODULE, c2s_broadcast_recipients, 75),
    ejabberd_hooks:add(user_send_packet, Host,
		       ?MODULE, user_send_packet, 75),
    ejabberd_hooks:add(user_receive_packet, Host,
		       ?MODULE, user_receive_packet, 75),
    ejabberd_hooks:add(c2s_stream_features, Host,
		       ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:add(s2s_stream_features, Host,
		       ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:add(disco_local_features, Host,
		       ?MODULE, disco_features, 75),
    ejabberd_hooks:add(disco_local_identity, Host,
		       ?MODULE, disco_identity, 75),
    ejabberd_hooks:add(disco_info, Host,
		       ?MODULE, disco_info, 75),
    {ok, #state{host = Host}}.
>>>>>>> upstream/master

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

<<<<<<< HEAD
handle_cast(_Msg, State) ->
    {noreply, State}.
=======
handle_cast(_Msg, State) -> {noreply, State}.
>>>>>>> upstream/master

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
<<<<<<< HEAD
    HostB = State#state.host,
    ejabberd_hooks:delete(c2s_presence_in, HostB,
			  ?MODULE, c2s_presence_in, 75),
    ejabberd_hooks:delete(c2s_broadcast_recipients, HostB,
			  ?MODULE, c2s_broadcast_recipients, 75),
    ejabberd_hooks:delete(user_send_packet, HostB,
			  ?MODULE, user_send_packet, 75),
    ejabberd_hooks:delete(user_receive_packet, HostB,
			  ?MODULE, user_receive_packet, 75),
    ejabberd_hooks:delete(c2s_stream_features, HostB,
			  ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(s2s_stream_features, HostB,
			  ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(disco_local_features, HostB,
			  ?MODULE, disco_features, 75),
    ejabberd_hooks:delete(disco_local_identity, HostB,
			  ?MODULE, disco_identity, 75),
    ejabberd_hooks:delete(disco_info, HostB,
			  ?MODULE, disco_info, 75),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
=======
    Host = State#state.host,
    ejabberd_hooks:delete(c2s_presence_in, Host, ?MODULE,
			  c2s_presence_in, 75),
    ejabberd_hooks:delete(c2s_broadcast_recipients, Host,
			  ?MODULE, c2s_broadcast_recipients, 75),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 75),
    ejabberd_hooks:delete(user_receive_packet, Host,
			  ?MODULE, user_receive_packet, 75),
    ejabberd_hooks:delete(c2s_stream_features, Host,
			  ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(s2s_stream_features, Host,
			  ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(disco_local_features, Host,
			  ?MODULE, disco_features, 75),
    ejabberd_hooks:delete(disco_local_identity, Host,
			  ?MODULE, disco_identity, 75),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE,
			  disco_info, 75),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
>>>>>>> upstream/master

%%====================================================================
%% Aux functions
%%====================================================================
<<<<<<< HEAD
feature_request(Host, From, Caps, [SubNode | Tail] = SubNodes) ->
    Node = Caps#caps.node,
    case cache_tab:lookup(caps_features, {Node, SubNode},
			  caps_read_fun({Node, SubNode})) of
	{ok, Fs} when is_list(Fs) ->
	    feature_request(Host, From, Caps, Tail);
	Other ->
	    NeedRequest = case Other of
			    {ok, TS} ->
				now_ts() >= TS + ?BAD_HASH_LIFETIME;
			    _ ->
				true
			  end,
	    if NeedRequest ->
		    IQ = #iq{type = 'get',
                             iq_ns = ?NS_JABBER_CLIENT,
			     ns = ?NS_DISCO_INFO,
			     payload = #xmlel{ns = ?NS_DISCO_INFO,
					      name = 'query',
					      attrs = [
						#xmlattr{name = <<"node">>,
							 value = <<Node/binary, <<"#">>/binary, SubNode/binary>>}]}},
		    F = fun(IQReply) ->
				feature_response(
				  IQReply, Host, From, Caps, SubNodes)
			end,
		    ejabberd_local:route_iq(
			exmpp_jid:make(Host), From, IQ, F);
		true ->
		    feature_request(Host, From, Caps, Tail)
	    end
    end;
feature_request(_Host, _From, _Caps, []) ->
    ok.

feature_response(#iq{type = result,
		     payload = #xmlel{children = Els}},
		 Host, From, Caps, [SubNode | SubNodes]) ->
    case check_hash(Caps, Els) of
	true ->
	    Features = lists:flatmap(
		fun(#xmlel{name = 'feature', attrs = FAttrs}) ->
		    case exmpp_xml:get_attribute_from_list(FAttrs, <<"var">>, <<>>) of
			<<>>    -> [];
			Feature -> [Feature]
		    end;
		   (_) ->
			[]
		end, Els),
	    cache_tab:insert(
		caps_features, {Caps#caps.node, SubNode}, Features,
		caps_write_fun({Caps#caps.node, SubNode}, Features));
	false ->
	    %% We cache current timestamp and will probe the client
	    %% after BAD_HASH_LIFETIME seconds.
	    cache_tab:insert(caps_features, {Caps#caps.node, SubNode}, now_ts(),
			     caps_write_fun({Caps#caps.node, SubNode}, now_ts()))
    end,
    feature_request(Host, From, Caps, SubNodes);
feature_response(_IQResult, Host, From, Caps, [SubNode | SubNodes]) ->
    %% We got type=error or invalid type=result stanza or timeout,
    %% so we cache current timestamp and will probe the client
    %% after BAD_HASH_LIFETIME seconds.
    cache_tab:insert(caps_features, {Caps#caps.node, SubNode}, now_ts(),
		     caps_write_fun({Caps#caps.node, SubNode}, now_ts())),
    feature_request(Host, From, Caps, SubNodes).

caps_read_fun(Node) ->
    fun() ->
	    case mnesia:dirty_read({caps_features, Node}) of
		[#caps_features{features = Features}] ->
		    {ok, Features};
		_ ->
		    error
=======
feature_request(Host, From, Caps,
		[SubNode | Tail] = SubNodes) ->
    Node = Caps#caps.node,
    NodePair = {Node, SubNode},
    case cache_tab:lookup(caps_features, NodePair,
			  caps_read_fun(NodePair))
	of
      {ok, Fs} when is_list(Fs) ->
	  feature_request(Host, From, Caps, Tail);
      Other ->
	  NeedRequest = case Other of
			  {ok, TS} -> now_ts() >= TS + (?BAD_HASH_LIFETIME);
			  _ -> true
			end,
	  if NeedRequest ->
		 IQ = #iq{type = get, xmlns = ?NS_DISCO_INFO,
			  sub_el =
			      [#xmlel{name = <<"query">>,
				      attrs =
					  [{<<"xmlns">>, ?NS_DISCO_INFO},
					   {<<"node">>,
					    <<Node/binary, "#",
					      SubNode/binary>>}],
				      children = []}]},
		 cache_tab:insert(caps_features, NodePair, now_ts(),
				  caps_write_fun(NodePair, now_ts())),
		 F = fun (IQReply) ->
			     feature_response(IQReply, Host, From, Caps,
					      SubNodes)
		     end,
		 ejabberd_local:route_iq(jlib:make_jid(<<"">>, Host,
						       <<"">>),
					 From, IQ, F);
	     true -> feature_request(Host, From, Caps, Tail)
	  end
    end;
feature_request(_Host, _From, _Caps, []) -> ok.

feature_response(#iq{type = result,
		     sub_el = [#xmlel{children = Els}]},
		 Host, From, Caps, [SubNode | SubNodes]) ->
    NodePair = {Caps#caps.node, SubNode},
    case check_hash(Caps, Els) of
      true ->
	  Features = lists:flatmap(fun (#xmlel{name =
						   <<"feature">>,
					       attrs = FAttrs}) ->
					   [xml:get_attr_s(<<"var">>, FAttrs)];
				       (_) -> []
				   end,
				   Els),
	  cache_tab:insert(caps_features, NodePair,
			   Features,
			   caps_write_fun(NodePair, Features));
      false -> ok
    end,
    feature_request(Host, From, Caps, SubNodes);
feature_response(_IQResult, Host, From, Caps,
		 [_SubNode | SubNodes]) ->
    feature_request(Host, From, Caps, SubNodes).

caps_read_fun(Node) ->
    fun () ->
	    case mnesia:dirty_read({caps_features, Node}) of
	      [#caps_features{features = Features}] -> {ok, Features};
	      _ -> error
>>>>>>> upstream/master
	    end
    end.

caps_write_fun(Node, Features) ->
<<<<<<< HEAD
    fun() ->
	    mnesia:dirty_write(
	      #caps_features{node_pair = Node,
			     features = Features})
    end.

make_my_disco_hash(Host) ->
    JID = exmpp_jid:make(Host),
    case {ejabberd_hooks:run_fold(disco_local_features,
				  Host,
				  empty,
				  [JID, JID, <<>>, <<>>]),
	  ejabberd_hooks:run_fold(disco_local_identity,
				  Host,
				  [],
				  [JID, JID, <<>>, <<>>]),
	  ejabberd_hooks:run_fold(disco_info,
				  Host,
				  [],
				  [Host, undefined, <<>>, <<>>])} of
	{{result, Features}, Identities, Info} ->
	    Feats = lists:map(
		      fun({{Feat, _Host}}) ->
			      #xmlel{name = feature,
				     attrs = [?XMLATTR(<<"var">>, Feat)]};
			 (Feat) ->
			      #xmlel{name = feature,
				     attrs = [?XMLATTR(<<"var">>, Feat)]}
		      end, Features),
	    make_disco_hash(Identities ++ Info ++ Feats, sha1);
	_Err ->
	    <<"">>
    end.

-ifdef(HAVE_MD2).
make_disco_hash(DiscoEls, Algo) ->
    Concat = [concat_identities(DiscoEls),
	      concat_features(DiscoEls),
	      concat_info(DiscoEls)],
    base64:encode(
      if Algo == md2 ->
	      sha:md2(Concat);
	 Algo == md5 ->
	      crypto:md5(Concat);
	 Algo == sha1 ->
	      crypto:sha(Concat);
	 Algo == sha224 ->
	      sha:sha224(Concat);
	 Algo == sha256 ->
	      sha:sha256(Concat);
	 Algo == sha384 ->
	      sha:sha384(Concat);
	 Algo == sha512 ->
	      sha:sha512(Concat)
      end).

check_hash(Caps, Els) ->
    case Caps#caps.hash of
	<<"md2">> ->
	    Caps#caps.version == make_disco_hash(Els, md2);
	<<"md5">> ->
	    Caps#caps.version == make_disco_hash(Els, md5);
	<<"sha-1">> ->
	    Caps#caps.version == make_disco_hash(Els, sha1);
	<<"sha-224">> ->
	    Caps#caps.version == make_disco_hash(Els, sha224);
	<<"sha-256">> ->
	    Caps#caps.version == make_disco_hash(Els, sha256);
	<<"sha-384">> ->
	    Caps#caps.version == make_disco_hash(Els, sha384);
	<<"sha-512">> ->
	    Caps#caps.version == make_disco_hash(Els, sha512);
	_ ->
	    true
    end.
-else.
make_disco_hash(DiscoEls, Algo) ->
    Concat = [concat_identities(DiscoEls),
	      concat_features(DiscoEls),
	      concat_info(DiscoEls)],
    base64:encode(
      if Algo == md5 ->
	      crypto:md5(Concat);
	 Algo == sha1 ->
	      crypto:sha(Concat);
	 Algo == sha224 ->
	      sha:sha224(Concat);
	 Algo == sha256 ->
	      sha:sha256(Concat);
	 Algo == sha384 ->
	      sha:sha384(Concat);
	 Algo == sha512 ->
	      sha:sha512(Concat)
      end).

check_hash(Caps, Els) ->
    case Caps#caps.hash of
	<<"md5">> ->
	    Caps#caps.version == make_disco_hash(Els, md5);
	<<"sha-1">> ->
	    Caps#caps.version == make_disco_hash(Els, sha1);
	<<"sha-224">> ->
	    Caps#caps.version == make_disco_hash(Els, sha224);
	<<"sha-256">> ->
	    Caps#caps.version == make_disco_hash(Els, sha256);
	<<"sha-384">> ->
	    Caps#caps.version == make_disco_hash(Els, sha384);
	<<"sha-512">> ->
	    Caps#caps.version == make_disco_hash(Els, sha512);
	_ ->
	    true
    end.
-endif.

concat_features(Els) ->
    lists:usort(
      lists:flatmap(
	fun(#xmlel{name = feature} = El) ->
		[[exmpp_xml:get_attribute(El, <<"var">>, <<>>), $<]];
	   (_) ->
		[]
	end, Els)).

concat_identities(Els) ->
    lists:sort(
      lists:flatmap(
	fun(#xmlel{name = identity} = El) ->
		[[exmpp_xml:get_attribute_as_binary(El, <<"category">>, <<>>), $/,
		  exmpp_xml:get_attribute_as_binary(El, <<"type">>, <<>>), $/,
		  exmpp_xml:get_attribute_as_binary(El, <<"lang">>, <<>>), $/,
		  exmpp_xml:get_attribute_as_binary(El, <<"name">>, <<>>), $<]];
	   (_) ->
		[]
	end, Els)).

concat_info(Els) ->
    lists:sort(
      lists:flatmap(
	fun(#xmlel{name = x, ns = ?NS_DATA_FORMS, children = Fields} = El) ->
		case exmpp_xml:get_attribute(El, <<"type">>, <<>>) of
		    <<"result">> ->
			[concat_xdata_fields(Fields)];
		    _ ->
			[]
		end;
	   (_) ->
		[]
	end, Els)).

concat_xdata_fields(Fields) ->
    [Form, Res] =
	lists:foldl(
	  fun(#xmlel{name = field, children = Els} = El,
	      [FormType, VarFields] = Acc) ->
		  case exmpp_xml:get_attribute_as_binary(El, <<"var">>, <<>>) of
		      <<>> ->
			  Acc;
		      <<"FORM_TYPE">> ->
			  [exmpp_xml:get_path(
			     El, [{element, value}, cdata]), VarFields];
		      Var ->
			  [FormType,
			   [[[Var, $<],
			     lists:sort(
			       lists:flatmap(
				 fun(#xmlel{name = value} = VEl) ->
					 [[exmpp_xml:get_cdata(VEl), $<]];
				    (_) ->
					 []
				 end, Els))] | VarFields]]
		  end;
	     (_, Acc) ->
		  Acc
	  end, [<<>>, []], Fields),
=======
    fun () ->
	    mnesia:dirty_write(#caps_features{node_pair = Node,
					      features = Features})
    end.

make_my_disco_hash(Host) ->
    JID = jlib:make_jid(<<"">>, Host, <<"">>),
    case {ejabberd_hooks:run_fold(disco_local_features,
				  Host, empty, [JID, JID, <<"">>, <<"">>]),
	  ejabberd_hooks:run_fold(disco_local_identity, Host, [],
				  [JID, JID, <<"">>, <<"">>]),
	  ejabberd_hooks:run_fold(disco_info, Host, [],
				  [Host, undefined, <<"">>, <<"">>])}
	of
      {{result, Features}, Identities, Info} ->
	  Feats = lists:map(fun ({{Feat, _Host}}) ->
				    #xmlel{name = <<"feature">>,
					   attrs = [{<<"var">>, Feat}],
					   children = []};
				(Feat) ->
				    #xmlel{name = <<"feature">>,
					   attrs = [{<<"var">>, Feat}],
					   children = []}
			    end,
			    Features),
	  make_disco_hash(Identities ++ Info ++ Feats, sha1);
      _Err -> <<"">>
    end.

make_disco_hash(DiscoEls, Algo) ->
    Concat = list_to_binary([concat_identities(DiscoEls),
                             concat_features(DiscoEls), concat_info(DiscoEls)]),
    jlib:encode_base64(case Algo of
                           md5 -> erlang:md5(Concat);
                           sha1 -> p1_sha:sha1(Concat);
                           sha224 -> p1_sha:sha224(Concat);
                           sha256 -> p1_sha:sha256(Concat);
                           sha384 -> p1_sha:sha384(Concat);
                           sha512 -> p1_sha:sha512(Concat)
                       end).

check_hash(Caps, Els) ->
    case Caps#caps.hash of
      <<"md5">> ->
	  Caps#caps.version == make_disco_hash(Els, md5);
      <<"sha-1">> ->
	  Caps#caps.version == make_disco_hash(Els, sha1);
      <<"sha-224">> ->
	  Caps#caps.version == make_disco_hash(Els, sha224);
      <<"sha-256">> ->
	  Caps#caps.version == make_disco_hash(Els, sha256);
      <<"sha-384">> ->
	  Caps#caps.version == make_disco_hash(Els, sha384);
      <<"sha-512">> ->
	  Caps#caps.version == make_disco_hash(Els, sha512);
      _ -> true
    end.

concat_features(Els) ->
    lists:usort(lists:flatmap(fun (#xmlel{name =
					      <<"feature">>,
					  attrs = Attrs}) ->
				      [[xml:get_attr_s(<<"var">>, Attrs), $<]];
				  (_) -> []
			      end,
			      Els)).

concat_identities(Els) ->
    lists:sort(lists:flatmap(fun (#xmlel{name =
					     <<"identity">>,
					 attrs = Attrs}) ->
				     [[xml:get_attr_s(<<"category">>, Attrs),
				       $/, xml:get_attr_s(<<"type">>, Attrs),
				       $/,
				       xml:get_attr_s(<<"xml:lang">>, Attrs),
				       $/, xml:get_attr_s(<<"name">>, Attrs),
				       $<]];
				 (_) -> []
			     end,
			     Els)).

concat_info(Els) ->
    lists:sort(lists:flatmap(fun (#xmlel{name = <<"x">>,
					 attrs = Attrs, children = Fields}) ->
				     case {xml:get_attr_s(<<"xmlns">>, Attrs),
					   xml:get_attr_s(<<"type">>, Attrs)}
					 of
				       {?NS_XDATA, <<"result">>} ->
					   [concat_xdata_fields(Fields)];
				       _ -> []
				     end;
				 (_) -> []
			     end,
			     Els)).

concat_xdata_fields(Fields) ->
    [Form, Res] = lists:foldl(fun (#xmlel{name =
					      <<"field">>,
					  attrs = Attrs, children = Els} =
				       El,
				   [FormType, VarFields] = Acc) ->
				      case xml:get_attr_s(<<"var">>, Attrs) of
					<<"">> -> Acc;
					<<"FORM_TYPE">> ->
					    [xml:get_subtag_cdata(El,
								  <<"value">>),
					     VarFields];
					Var ->
					    [FormType,
					     [[[Var, $<],
					       lists:sort(lists:flatmap(fun
									  (#xmlel{name
										      =
										      <<"value">>,
										  children
										      =
										      VEls}) ->
									      [[xml:get_cdata(VEls),
										$<]];
									  (_) ->
									      []
									end,
									Els))]
					      | VarFields]]
				      end;
				  (_, Acc) -> Acc
			      end,
			      [<<"">>, []], Fields),
>>>>>>> upstream/master
    [Form, $<, lists:sort(Res)].

gb_trees_fold(F, Acc, Tree) ->
    Iter = gb_trees:iterator(Tree),
    gb_trees_fold_iter(F, Acc, Iter).

gb_trees_fold_iter(F, Acc, Iter) ->
    case gb_trees:next(Iter) of
<<<<<<< HEAD
	{Key, Val, NewIter} ->
	    NewAcc = F(Key, Val, Acc),
	    gb_trees_fold_iter(F, NewAcc, NewIter);
	_ ->
	    Acc
    end.

now_ts() ->
    {MegaSecs, Secs, _} = now(),
    MegaSecs*1000000 + Secs.
=======
      {Key, Val, NewIter} ->
	  NewAcc = F(Key, Val, Acc),
	  gb_trees_fold_iter(F, NewAcc, NewIter);
      _ -> Acc
    end.

now_ts() ->
    {MegaSecs, Secs, _} = now(), MegaSecs * 1000000 + Secs.

is_valid_node(Node) ->
    case str:tokens(Node, <<"#">>) of
        [?EJABBERD_URI|_] ->
            true;
        _ ->
            false
    end.
>>>>>>> upstream/master
