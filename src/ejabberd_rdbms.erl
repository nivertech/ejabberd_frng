%%%----------------------------------------------------------------------
%%% File    : ejabberd_rdbms.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Manage the start of the database modules when needed
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_rdbms).

-author('alexey@process-one.net').

<<<<<<< HEAD
-export([start/0
         ,start_odbc/1
         ,stop_odbc/1
         ,running/1
        ]).
-include("ejabberd.hrl").
-include("ejabberd_config.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-define(SUPERVISOR, ejabberd_sup).
=======
-export([start/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
>>>>>>> upstream/master

start() ->
    case lists:any(fun(H) -> needs_odbc(H) /= false end,
                   ?MYHOSTS) of
        true ->
            start_hosts();
        false ->
            ok
    end.

%% Start relationnal DB module on the nodes where it is needed
start_hosts() ->
    lists:foreach(fun (Host) ->
			  case needs_odbc(Host) of
			    {true, App} -> start_odbc(Host, App);
			    false -> ok
			  end
		  end,
		  ?MYHOSTS).

%% Start the ODBC module on the given host
<<<<<<< HEAD
start_odbc(Host) ->
    SupervisorName = sup_name(Host),
    ChildSpec =
	{SupervisorName,
	 {ejabberd_odbc_sup, start_link, [Host]},
	 transient,
	 infinity,
	 supervisor,
	 [ejabberd_odbc_sup]},
    case supervisor:start_child(?SUPERVISOR, ChildSpec) of
	{ok, _PID} ->
	    ok;
	_Error ->
	    ?ERROR_MSG("Start of supervisor ~p failed:~n~p~nRetrying...~n", [SupervisorName, _Error]),
	    start_odbc(Host)
    end.

stop_odbc(Host) ->
    SupervisorName = sup_name(Host),
    case running(Host) of
	false -> ok;
	true ->
	    case [H || H <- dependent_hosts(Host), ejabberd_hosts:running(H)] of
		[] ->
		    ?INFO_MSG("About to terminate ~p", [SupervisorName]),
		    ok = supervisor:terminate_child(?SUPERVISOR, SupervisorName),
		    ok = supervisor:delete_child(?SUPERVISOR, SupervisorName);
		RunningHosts ->
		    ?WARNING_MSG("Not stopping ODBC for ~p because the virtual hosts ~p are still using it.",
		    [Host, RunningHosts]),
		    {error, still_in_use}
	    end
    end.

%% Returns true if we have configured odbc_server for the given host
needs_odbc(Host) ->
    try
	LHost = exmpp_stringprep:nameprep(Host),
	case ejabberd_config:get_local_option({odbc_server, LHost}) of
	    undefined ->
		false;
	    {host, _} ->
		false;
	    _ ->
		true
	end
    catch
	_ ->
	    false
    end.

running(Host) ->
    Supervisors = supervisor:which_children(?SUPERVISOR),
    SupervisorName = gen_mod:get_module_proc(Host, ejabberd_odbc_sup),
    case lists:keysearch(SupervisorName, 1, Supervisors) of
	false -> false;
	{value, Cspec} when is_tuple(Cspec) -> true
=======
start_odbc(Host, App) ->
    ejabberd:start_app(App),
    Supervisor_name = gen_mod:get_module_proc(Host,
					      ejabberd_odbc_sup),
    ChildSpec = {Supervisor_name,
		 {ejabberd_odbc_sup, start_link, [Host]}, transient,
		 infinity, supervisor, [ejabberd_odbc_sup]},
    case supervisor:start_child(ejabberd_sup, ChildSpec) of
      {ok, _PID} -> ok;
      _Error ->
	  ?ERROR_MSG("Start of supervisor ~p failed:~n~p~nRetrying."
		     "..~n",
		     [Supervisor_name, _Error]),
	  start_odbc(Host, App)
    end.

%% Returns {true, App} if we have configured odbc for the given host
needs_odbc(Host) ->
    LHost = jlib:nameprep(Host),
    case ejabberd_config:get_option({odbc_type, LHost},
                                    fun(mysql) -> mysql;
                                       (pgsql) -> pgsql;
                                       (odbc) -> odbc
                                    end, undefined) of
        mysql -> {true, p1_mysql};
        pgsql -> {true, p1_pgsql};
        odbc -> {true, odbc};
        undefined -> false
>>>>>>> upstream/master
    end.


dependent_hosts(Host) ->
    MS = ets:fun2ms(fun (#local_config{key={odbc_server, DHost},
                                       value={host, H}})
                        when H =:= Host ->
                            DHost
                    end),
    ejabberd_config:search(MS).

sup_name(Host) ->
    gen_mod:get_module_proc(Host, ejabberd_odbc_sup).
