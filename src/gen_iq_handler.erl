%%%----------------------------------------------------------------------
%%% File    : gen_iq_handler.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IQ handler support
%%% Created : 22 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(gen_iq_handler).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/3, add_iq_handler/6,
	 remove_iq_handler/3, stop_iq_handler/3, handle/7,
	 process_iq/6, check_type/1, transform_module_options/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(state, {host, module, function}).

-type component() :: ejabberd_sm | ejabberd_local.
-type type() :: no_queue | one_queue | pos_integer() | parallel.
-type opts() :: no_queue | {one_queue, pid()} | {queues, [pid()]} | parallel.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Module, Function) ->
    gen_server:start_link(?MODULE, [Host, Module, Function],
			  []).

add_iq_handler(Component, Host, NS, Module, Function,
	       Type) ->
    case Type of
      no_queue ->
	  Component:register_iq_handler(Host, NS, Module,
					Function, no_queue);
      one_queue ->
	  {ok, Pid} = supervisor:start_child(ejabberd_iq_sup,
					     [Host, Module, Function]),
	  Component:register_iq_handler(Host, NS, Module,
					Function, {one_queue, Pid});
      N when is_integer(N) ->
	  Pids = lists:map(fun (_) ->
				   {ok, Pid} =
				       supervisor:start_child(ejabberd_iq_sup,
							      [Host, Module,
							       Function]),
				   Pid
			   end,
			   lists:seq(1, N)),
	  Component:register_iq_handler(Host, NS, Module,
					Function, {queues, Pids});
      parallel ->
	  Component:register_iq_handler(Host, NS, Module,
					Function, parallel)
    end.

remove_iq_handler(Component, Host, NS) ->
    Component:unregister_iq_handler(Host, NS).

stop_iq_handler(_Module, _Function, Opts) ->
    case Opts of
      {one_queue, Pid} -> gen_server:call(Pid, stop);
      {queues, Pids} ->
	  lists:foreach(fun (Pid) ->
				catch gen_server:call(Pid, stop)
			end,
			Pids);
      _ -> ok
    end.

handle(Host, Module, Function, Opts, From, To, IQ_Rec) ->
    case Opts of
<<<<<<< HEAD
	no_queue ->
	    process_iq(Host, Module, Function, From, To, IQ_Rec);
	{one_queue, Pid} ->
	    Pid ! {process_iq, From, To, IQ_Rec};
	{queues, Pids} ->
	    Pid = lists:nth(erlang:phash(now(), length(Pids)), Pids),
	    Pid ! {process_iq, From, To, IQ_Rec};
	parallel ->
	    spawn(?MODULE, process_iq,
	      [Host, Module, Function, From, To, IQ_Rec]);
	_ ->
	    todo
    end.


process_iq(_Host, Module, Function, From, To, IQ_Rec) ->
    try Module:Function(From, To, IQ_Rec) of
	ignore ->
	    ok;
	ResIQ ->
	    Reply = exmpp_iq:iq_to_xmlel(ResIQ, To, From),
	    ejabberd_router:route(To, From, Reply)
    catch
	_Class:Reason ->
	    ?ERROR_MSG("~s:~s/3 crashed: ~p~n~p~n",
              [Module, Function, Reason, erlang:get_stacktrace()])
=======
      no_queue ->
	  process_iq(Host, Module, Function, From, To, IQ);
      {one_queue, Pid} -> Pid ! {process_iq, From, To, IQ};
      {queues, Pids} ->
	  Pid = lists:nth(erlang:phash(now(), length(Pids)),
			  Pids),
	  Pid ! {process_iq, From, To, IQ};
      parallel ->
	  spawn(?MODULE, process_iq,
		[Host, Module, Function, From, To, IQ]);
      _ -> todo
    end.

process_iq(_Host, Module, Function, From, To, IQ) ->
    case catch Module:Function(From, To, IQ) of
      {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]);
      ResIQ ->
	  if ResIQ /= ignore ->
		 ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
	     true -> ok
	  end
>>>>>>> upstream/master
    end.

-spec check_type(type()) -> type().

check_type(no_queue) -> no_queue;
check_type(one_queue) -> one_queue;
check_type(N) when is_integer(N), N>0 -> N;
check_type(parallel) -> parallel.

-spec transform_module_options([{atom(), any()}]) -> [{atom(), any()}].

transform_module_options(Opts) ->
    lists:map(
      fun({iqdisc, {queues, N}}) ->
              {iqdisc, N};
         (Opt) ->
              Opt
      end, Opts).

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
init([Host, Module, Function]) ->
    {ok,
     #state{host = Host, module = Module,
	    function = Function}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    Reply = ok, {stop, normal, Reply, State}.

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
<<<<<<< HEAD
handle_info({process_iq, From, To, IQ_Rec},
	    #state{host = Host,
		   module = Module,
		   function = Function} = State) ->
    process_iq(Host, Module, Function, From, To, IQ_Rec),
=======
handle_info({process_iq, From, To, IQ},
	    #state{host = Host, module = Module,
		   function = Function} =
		State) ->
    process_iq(Host, Module, Function, From, To, IQ),
>>>>>>> upstream/master
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

