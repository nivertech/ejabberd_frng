%%%----------------------------------------------------------------------
%%% File    : extauth.erl
%%% Author  : Leif Johansson <leifj@it.su.se>
%%% Purpose : External authentication using a simple port-driver
%%% Created : 30 Jul 2004 by Leif Johansson <leifj@it.su.se>
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

-module(extauth).

-author('leifj@it.su.se').

<<<<<<< HEAD
-export([start/2,
	 stop/1,
	 init/2,
	 check_password/3,
	 set_password/3,
	 try_register/3,
	 remove_user/2,
	 remove_user/3,
	 is_user_exists/2]).
=======
-export([start/2, stop/1, init/2, check_password/3,
	 set_password/3, try_register/3, remove_user/2,
	 remove_user/3, is_user_exists/2]).
>>>>>>> upstream/master

-include("ejabberd.hrl").
-include("logger.hrl").

-define(INIT_TIMEOUT, 60000).

-define(CALL_TIMEOUT, 10000).

start(Host, ExtPrg) ->
<<<<<<< HEAD
    lists:foreach(
	fun(This) ->
	    start_instance(get_process_name(Host, This), ExtPrg)
	end,
	lists:seq(0, get_instances(Host)-1)
    ).
=======
    lists:foreach(fun (This) ->
			  start_instance(get_process_name(Host, This), ExtPrg)
		  end,
		  lists:seq(0, get_instances(Host) - 1)).
>>>>>>> upstream/master

start_instance(ProcessName, ExtPrg) ->
    spawn(?MODULE, init, [ProcessName, ExtPrg]).

restart_instance(ProcessName, ExtPrg) ->
    unregister(ProcessName),
    start_instance(ProcessName, ExtPrg).

init(ProcessName, ExtPrg) ->
    register(ProcessName, self()),
<<<<<<< HEAD
    process_flag(trap_exit,true),
    Port = open_port({spawn, ExtPrg}, [{packet,2}]),
    loop(Port, ?INIT_TIMEOUT, ProcessName, ExtPrg).

stop(Host) ->
    lists:foreach(
	fun(This) ->
	    get_process_name(Host, This) ! stop
	end,
	lists:seq(0, get_instances(Host)-1)
    ).

get_process_name(Host, Integer) ->
    gen_mod:get_module_proc(lists:append([Host, integer_to_list(Integer)]), eauth).
=======
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port, ?INIT_TIMEOUT, ProcessName, ExtPrg).

stop(Host) ->
    lists:foreach(fun (This) ->
			  get_process_name(Host, This) ! stop
		  end,
		  lists:seq(0, get_instances(Host) - 1)).

get_process_name(Host, Integer) ->
    gen_mod:get_module_proc(iolist_to_binary([Host,
                                              integer_to_list(Integer)]),
			    eauth).
>>>>>>> upstream/master

check_password(User, Server, Password) ->
    call_port(Server, [<<"auth">>, User, Server, Password]).

is_user_exists(User, Server) ->
    call_port(Server, [<<"isuser">>, User, Server]).

set_password(User, Server, Password) ->
    call_port(Server, [<<"setpass">>, User, Server, Password]).

try_register(User, Server, Password) ->
    case call_port(Server,
		   [<<"tryregister">>, User, Server, Password])
	of
      true -> {atomic, ok};
      false -> {error, not_allowed}
    end.

remove_user(User, Server) ->
    call_port(Server, [<<"removeuser">>, User, Server]).

remove_user(User, Server, Password) ->
    call_port(Server,
	      [<<"removeuser3">>, User, Server, Password]).

try_register(User, Server, Password) ->
    case call_port(Server, ["tryregister", User, Server, Password]) of
	true -> {atomic, ok};
	false -> {error, not_allowed}
    end.

remove_user(User, Server) ->
    call_port(Server, ["removeuser", User, Server]).

remove_user(User, Server, Password) ->
    call_port(Server, ["removeuser3", User, Server, Password]).

call_port(Server, Msg) ->
<<<<<<< HEAD
    LServer = exmpp_stringprep:nameprep(Server),
    ProcessName = get_process_name(LServer, random_instance(get_instances(LServer))),
    ProcessName ! {call, self(), Msg},
=======
    LServer = jlib:nameprep(Server),
    ProcessName = get_process_name(LServer,
				   random_instance(get_instances(LServer))),
    ProcessName ! {call, self(), Msg},
    receive {eauth, Result} -> Result end.

random_instance(MaxNum) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(MaxNum) - 1.

get_instances(Server) ->
    ejabberd_config:get_option(
      {extauth_instances, Server},
      fun(V) when is_integer(V), V > 0 ->
              V
      end, 1).

loop(Port, Timeout, ProcessName, ExtPrg) ->
>>>>>>> upstream/master
    receive
      {call, Caller, Msg} ->
	  port_command(Port, encode(Msg)),
	  receive
	    {Port, {data, Data}} ->
		?DEBUG("extauth call '~p' received data response:~n~p",
		       [Msg, Data]),
		Caller ! {eauth, decode(Data)},
		loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg);
	    {Port, Other} ->
		?ERROR_MSG("extauth call '~p' received strange response:~n~p",
			   [Msg, Other]),
		Caller ! {eauth, false},
		loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg)
	    after Timeout ->
		      ?ERROR_MSG("extauth call '~p' didn't receive response",
				 [Msg]),
		      Caller ! {eauth, false},
		      Pid = restart_instance(ProcessName, ExtPrg),
		      flush_buffer_and_forward_messages(Pid),
		      exit(port_terminated)
	  end;
      stop ->
	  Port ! {self(), close},
	  receive {Port, closed} -> exit(normal) end;
      {'EXIT', Port, Reason} ->
	  ?CRITICAL_MSG("extauth script has exitted abruptly "
			"with reason '~p'",
			[Reason]),
	  Pid = restart_instance(ProcessName, ExtPrg),
	  flush_buffer_and_forward_messages(Pid),
	  exit(port_terminated)
    end.

<<<<<<< HEAD
random_instance(MaxNum) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(MaxNum) - 1.

get_instances(Server) ->
    case ejabberd_config:get_local_option({extauth_instances, Server}) of
	Num when is_integer(Num) -> Num;
	_ -> 1
    end.

loop(Port, Timeout, ProcessName, ExtPrg) ->
    receive
	{call, Caller, Msg} ->
	    port_command(Port, encode(Msg)),
	    receive
		{Port, {data, Data}} ->
                    ?DEBUG("extauth call '~p' received data response:~n~p", [Msg, Data]),
		    Caller ! {eauth, decode(Data)},
		    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg);
		{Port, Other} ->
                    ?ERROR_MSG("extauth call '~p' received strange response:~n~p", [Msg, Other]),
		    Caller ! {eauth, false},
		    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg)
            after
                Timeout ->
                    ?ERROR_MSG("extauth call '~p' didn't receive response", [Msg]),
		    Caller ! {eauth, false},
		    Pid = restart_instance(ProcessName, ExtPrg),
		    flush_buffer_and_forward_messages(Pid),
		    exit(port_terminated)
	    end;
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    ?CRITICAL_MSG("extauth script has exitted abruptly with reason '~p'", [Reason]),
	    Pid = restart_instance(ProcessName, ExtPrg),
	    flush_buffer_and_forward_messages(Pid),
	    exit(port_terminated)
    end.

flush_buffer_and_forward_messages(Pid) ->
    receive
	Message ->
	    Pid ! Message,
	    flush_buffer_and_forward_messages(Pid)
    after 0 ->
	    true
    end.

join(List, Sep) ->
    lists:foldl(fun(A, "") -> A;
		   (A, Acc) -> Acc ++ Sep ++ A
		end, "", List).

encode(L) ->
    join(L,":").

decode([0,0]) ->
    false;
decode([0,1]) ->
    true.
=======
flush_buffer_and_forward_messages(Pid) ->
    receive
      Message ->
	  Pid ! Message, flush_buffer_and_forward_messages(Pid)
      after 0 -> true
    end.

encode(L) -> str:join(L, <<":">>).
>>>>>>> upstream/master

decode([0, 0]) -> false;
decode([0, 1]) -> true.
