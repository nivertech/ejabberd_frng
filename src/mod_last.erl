%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:last support (XEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%% 2.1.x / mnesia / last_activity
%%%  us = {Username::string(), Host::string()}
%%%  timestamp = now()
%%%  status = string()
%%%
%%% 2.1.x / odbc / last
%%%  username = varchar250
%%%  seconds = text
%%%  state = text
%%%
%%% 3.0.0-prealpha / mnesia / last_activity
%%%  us = {Username::binary(), Host::binary()}
%%%  timestamp = now()
%%%  status = binary()
%%%
%%% 3.0.0-prealpha / odbc / last
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / last_activity
%%%  user_host = {Username::binary(), Host::binary()}
%%%  timestamp = now()
%%%  status = binary()
%%%
%%% 3.0.0-alpha / odbc / last_activity
%%%  user = varchar150
%%%  host = varchar150
%%%  timestamp = bigint
%%%  status = text

-module(mod_last).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3, export/1,
	 process_sm_iq/3, on_presence_update/4, import/1, import/3,
	 store_last_info/4, get_last_info/2, remove_user/2,
         transform_options/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD
-include("mod_privacy.hrl").

-record(last_activity, {user_host, timestamp, status}).
=======
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").
>>>>>>> upstream/master

-record(last_activity, {us = {<<"">>, <<"">>} :: {binary(), binary()},
                        timestamp = 0 :: non_neg_integer(),
                        status = <<"">> :: binary()}).

<<<<<<< HEAD
start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, HostB, last_activity,
			     [{disc_copies, [node()]},
			      {odbc_host, HostB},
			      {attributes, record_info(fields, last_activity)},
			      {types, [{user_host, {text, text}},
				       {timestamp, bigint}]}]),
    update_table(HostB, Backend),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_LAST_ACTIVITY,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_LAST_ACTIVITY,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, HostB,
		       ?MODULE, on_presence_update, 50).

stop(Host) when is_list(Host) ->
    stop(list_to_binary(Host));
stop(HostB) ->
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, HostB,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_LAST_ACTIVITY),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_LAST_ACTIVITY).
=======
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    case gen_mod:db_type(Opts) of
      mnesia ->
	  mnesia:create_table(last_activity,
			      [{disc_copies, [node()]},
			       {attributes,
				record_info(fields, last_activity)}]),
	  update_table();
      _ -> ok
    end,
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_LAST, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_LAST, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
		       on_presence_update, 50).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_LAST).
>>>>>>> upstream/master

%%%
%%% Uptime of ejabberd node
%%%

<<<<<<< HEAD
process_local_iq(_From, _To, #iq{type = get} = IQ_Rec) ->
    Sec = get_node_uptime(),
    Response = #xmlel{ns = ?NS_LAST_ACTIVITY, name = 'query', attrs =
      [?XMLATTR(<<"seconds">>, Sec)]},
    exmpp_iq:result(IQ_Rec, Response);
process_local_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').
=======
process_local_iq(_From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  Sec = get_node_uptime(),
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs =
				[{<<"xmlns">>, ?NS_LAST},
				 {<<"seconds">>,
				  iolist_to_binary(integer_to_list(Sec))}],
			    children = []}]}
    end.
>>>>>>> upstream/master

%% @spec () -> integer()
%% @doc Get the uptime of the ejabberd node, expressed in seconds.
%% When ejabberd is starting, ejabberd_config:start/0 stores the datetime.
get_node_uptime() ->
<<<<<<< HEAD
    case ejabberd_config:get_local_option(node_start) of
 {_, _, _} = StartNow ->
     now_to_seconds(now()) - now_to_seconds(StartNow);
 _undefined ->
     trunc(element(1, erlang:statistics(wall_clock))/1000)
=======
    case ejabberd_config:get_option(
           node_start,
           fun(S) when is_integer(S), S >= 0 -> S end) of
        undefined ->
            trunc(element(1, erlang:statistics(wall_clock)) / 1000);
        Now ->
            now_to_seconds(now()) - Now
>>>>>>> upstream/master
    end.

now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

%%%
%%% Serve queries about user last online
%%%

<<<<<<< HEAD
process_sm_iq(From, To, #iq{type = get} = IQ_Rec) ->
    {Subscription, _Groups} =
	ejabberd_hooks:run_fold(
	  roster_get_jid_info, exmpp_jid:prep_domain(To),
	  {none, []}, [exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To), From]),
	SameUser = exmpp_jid:bare_compare(From, To),
    if
		(Subscription == both) or (Subscription == from) or SameUser ->
	    UserListRecord = ejabberd_hooks:run_fold(
			       privacy_get_user_list, exmpp_jid:prep_domain(To),
			       #userlist{},
			       [exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To)]),
	    case ejabberd_hooks:run_fold(
		   privacy_check_packet, exmpp_jid:prep_domain(To),
		   allow,
		   [exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To), UserListRecord,
		    {To, From,
		     exmpp_presence:available()},
		    out]) of
		allow ->
		    get_last_iq(IQ_Rec, exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To));
		deny ->
		    exmpp_iq:error(IQ_Rec, 'forbidden')
	    end;
	true ->
	    exmpp_iq:error(IQ_Rec, 'forbidden')
    end;
process_sm_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

%% TODO: This function could use get_last_info/2
%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found | {error, Reason}
get_last(LUser, LServer) ->
    case catch gen_storage:dirty_read(LServer, last_activity, {LUser, LServer}) of
	{'EXIT', Reason} ->
	    {error, Reason};
	[] ->
	    not_found;
	[#last_activity{timestamp = TimeStamp, status = Status}] ->
	    {ok, TimeStamp, Status}
    end.

get_last_iq(IQ_Rec, LUser, LServer) ->
    case ejabberd_sm:get_user_resources(LUser, LServer) of
	[] ->
		get_last_iq_disconnected(IQ_Rec, LUser, LServer);
	_ ->
		Sec = 0,
	    #xmlel{ns = ?NS_LAST_ACTIVITY, name = 'query',
	      attrs = [?XMLATTR(<<"seconds">>, Sec)]}
    end.

get_last_iq_disconnected(IQ_Rec, LUser, LServer) ->
    case get_last(LUser, LServer) of
	{error, _Reason} ->
	    exmpp_iq:error(IQ_Rec, 'internal-server-error');
	not_found ->
	    exmpp_iq:error(IQ_Rec, 'service-unavailable');
	{ok, TimeStamp, Status} ->
	    TimeStamp2 = now_to_seconds(now()),
	    Sec = TimeStamp2 - TimeStamp,
	    Response = #xmlel{ns = ?NS_LAST_ACTIVITY, name = 'query',
	      attrs = [?XMLATTR(<<"seconds">>, Sec)],
	      children = [#xmlcdata{cdata = Status}]},
	    exmpp_iq:result(IQ_Rec, Response)
=======
process_sm_iq(From, To,
	      #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  User = To#jid.luser,
	  Server = To#jid.lserver,
	  {Subscription, _Groups} =
	      ejabberd_hooks:run_fold(roster_get_jid_info, Server,
				      {none, []}, [User, Server, From]),
	  if (Subscription == both) or (Subscription == from) or
	       (From#jid.luser == To#jid.luser) and
		 (From#jid.lserver == To#jid.lserver) ->
		 UserListRecord =
		     ejabberd_hooks:run_fold(privacy_get_user_list, Server,
					     #userlist{}, [User, Server]),
		 case ejabberd_hooks:run_fold(privacy_check_packet,
					      Server, allow,
					      [User, Server, UserListRecord,
					       {To, From,
						#xmlel{name = <<"presence">>,
						       attrs = [],
						       children = []}},
					       out])
		     of
		   allow -> get_last_iq(IQ, SubEl, User, Server);
		   deny ->
		       IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
		 end;
	     true ->
		 IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
	  end
    end.

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found | {error, Reason}
get_last(LUser, LServer) ->
    get_last(LUser, LServer,
	     gen_mod:db_type(LServer, ?MODULE)).

get_last(LUser, LServer, mnesia) ->
    case catch mnesia:dirty_read(last_activity,
				 {LUser, LServer})
	of
      {'EXIT', Reason} -> {error, Reason};
      [] -> not_found;
      [#last_activity{timestamp = TimeStamp,
		      status = Status}] ->
	  {ok, TimeStamp, Status}
    end;
get_last(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_last(LServer, Username) of
      {selected, [<<"seconds">>, <<"state">>], []} ->
	  not_found;
      {selected, [<<"seconds">>, <<"state">>],
       [[STimeStamp, Status]]} ->
	  case catch jlib:binary_to_integer(STimeStamp) of
	    TimeStamp when is_integer(TimeStamp) ->
		{ok, TimeStamp, Status};
	    Reason -> {error, {invalid_timestamp, Reason}}
	  end;
      Reason -> {error, {invalid_result, Reason}}
    end.

get_last_iq(IQ, SubEl, LUser, LServer) ->
    case ejabberd_sm:get_user_resources(LUser, LServer) of
      [] ->
	  case get_last(LUser, LServer) of
	    {error, _Reason} ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
	    not_found ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
	    {ok, TimeStamp, Status} ->
		TimeStamp2 = now_to_seconds(now()),
		Sec = TimeStamp2 - TimeStamp,
		IQ#iq{type = result,
		      sub_el =
			  [#xmlel{name = <<"query">>,
				  attrs =
				      [{<<"xmlns">>, ?NS_LAST},
				       {<<"seconds">>,
					iolist_to_binary(integer_to_list(Sec))}],
				  children = [{xmlcdata, Status}]}]}
	  end;
      _ ->
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs =
				[{<<"xmlns">>, ?NS_LAST},
				 {<<"seconds">>, <<"0">>}],
			    children = []}]}
>>>>>>> upstream/master
    end.

on_presence_update(User, Server, _Resource, Status) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    store_last_info(User, Server, TimeStamp, Status).

<<<<<<< HEAD
store_last_info(User, Server, TimeStamp, Status) 
        when is_binary(User), is_binary(Server) ->
    try
	US = {User, Server},
	F = fun() ->
 		gen_storage:write(Server,
 				  #last_activity{user_host = US,
						timestamp = TimeStamp,
						status = Status})
	    end,
        gen_storage:transaction(Server, last_activity, F)
    catch
	_ ->
	    ok
    end.

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, Timestamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) when is_list(LUser), is_list(LServer) ->
    get_last_info(list_to_binary(LUser), list_to_binary(LServer));
get_last_info(LUser, LServer) when is_binary(LUser), is_binary(LServer) ->
    case get_last(LUser, LServer) of
	{error, _Reason} ->
	    not_found;
	Res ->
	    Res
    end.

remove_user(User, Server) when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	F = fun() ->
		gen_storage:delete(LServer, {last_activity, US})
	    end,
	gen_storage:transaction(LServer, last_activity, F)
    catch
	_ ->
	    ok
    end.

update_table(global, Storage) ->
    [update_table(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_table(HostB, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      HostB, last_activity,
      [{last_activity, [us, timestamp, status],
	fun({last_activity, {U, S}, Timestamp, Status}) ->
		U1 = case U of
			 "" -> undefined;
			 V  -> V
		     end,
		#last_activity{user_host = {list_to_binary(U1),
					    list_to_binary(S)},
			       timestamp = Timestamp,
			       status = list_to_binary(Status)}
	end}]);
update_table(HostB, odbc) ->
    gen_storage_migration:migrate_odbc(
      HostB, [last_activity],
      [{"last", ["username", "seconds", "state"],
	fun(_, Username, STimeStamp, Status) ->
		case catch list_to_integer(STimeStamp) of
		    TimeStamp when is_integer(TimeStamp) ->
			[#last_activity{user_host = {Username, HostB},
					timestamp = TimeStamp,
					status = Status}];
		    _ ->
			?WARNING_MSG("Omitting last_activity migration item"
				     " with timestamp=~p",
				     [STimeStamp])
		end
	end}]).
=======
store_last_info(User, Server, TimeStamp, Status) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    DBType = gen_mod:db_type(LServer, ?MODULE),
    store_last_info(LUser, LServer, TimeStamp, Status,
		    DBType).

store_last_info(LUser, LServer, TimeStamp, Status,
		mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:write(#last_activity{us = US,
					    timestamp = TimeStamp,
					    status = Status})
	end,
    mnesia:transaction(F);
store_last_info(LUser, LServer, TimeStamp, Status,
		odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    Seconds =
	ejabberd_odbc:escape(iolist_to_binary(integer_to_list(TimeStamp))),
    State = ejabberd_odbc:escape(Status),
    odbc_queries:set_last_t(LServer, Username, Seconds,
			    State).

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) ->
    case get_last(LUser, LServer) of
      {error, _Reason} -> not_found;
      Res -> Res
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    DBType = gen_mod:db_type(LServer, ?MODULE),
    remove_user(LUser, LServer, DBType).

remove_user(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:delete({last_activity, US}) end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_last(LServer, Username).

update_table() ->
    Fields = record_info(fields, last_activity),
    case mnesia:table_info(last_activity, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            last_activity, Fields, set,
            fun(#last_activity{us = {U, _}}) -> U end,
            fun(#last_activity{us = {U, S}, status = Status} = R) ->
                    R#last_activity{us = {iolist_to_binary(U),
                                          iolist_to_binary(S)},
                                    status = iolist_to_binary(Status)}
            end);
      _ ->
	  ?INFO_MSG("Recreating last_activity table", []),
	  mnesia:transform_table(last_activity, ignore, Fields)
    end.

export(_Server) ->
    [{last_activity,
      fun(Host, #last_activity{us = {LUser, LServer},
                               timestamp = TimeStamp, status = Status})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              Seconds =
                  ejabberd_odbc:escape(jlib:integer_to_binary(TimeStamp)),
              State = ejabberd_odbc:escape(Status),
              [[<<"delete from last where username='">>, Username, <<"';">>],
               [<<"insert into last(username, seconds, "
                  "state) values ('">>,
                Username, <<"', '">>, Seconds, <<"', '">>, State,
                <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, seconds, state from last">>,
      fun([LUser, TimeStamp, State]) ->
              #last_activity{us = {LUser, LServer},
                             timestamp = jlib:binary_to_integer(
                                           TimeStamp),
                             status = State}
      end}].

import(_LServer, mnesia, #last_activity{} = LA) ->
    mnesia:dirty_write(LA);
import(_, _, _) ->
    pass.

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({node_start, {_, _, _} = Now}, Opts) ->
    ?WARNING_MSG("Old 'node_start' format detected. This is still supported "
                 "but it is better to fix your config.", []),
    [{node_start, now_to_seconds(Now)}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].
>>>>>>> upstream/master
