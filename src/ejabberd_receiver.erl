%%%----------------------------------------------------------------------
%%% File    : ejabberd_receiver.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket receiver for C2S and S2S connections
%%% Created : 10 Nov 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_receiver).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/4,
	 start/3,
	 start/4,
	 change_shaper/2,
	 reset_stream/1,
	 starttls/2,
	 starttls/3,
	 compress/2,
	 send/2,
	 become_controller/2,
	 change_controller/2,
	 setopts/2,
	 close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

<<<<<<< HEAD
-record(state, {socket,
		sock_mod,
		shaper_state,
		c2s_pid,
		max_stanza_size,
		xml_stream_state,
		tref,
		timeout}).
=======
-record(state,
	{socket :: inet:socket() | p1_tls:tls_socket() | ezlib:zlib_socket(),
         sock_mod = gen_tcp :: gen_tcp | p1_tls | ezlib,
         shaper_state = none :: shaper:shaper(),
         c2s_pid :: pid(),
	 max_stanza_size = infinity :: non_neg_integer() | infinity,
         xml_stream_state :: xml_stream:xml_stream_state(),
         timeout = infinity:: timeout()}).
>>>>>>> upstream/master

-define(HIBERNATE_TIMEOUT, 90000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link(inet:socket(), atom(), shaper:shaper(),
                 non_neg_integer() | infinity) -> ignore |
                                                  {error, any()} |
                                                  {ok, pid()}.

start_link(Socket, SockMod, Shaper, MaxStanzaSize) ->
    gen_server:start_link(?MODULE,
			  [Socket, SockMod, Shaper, MaxStanzaSize], []).

-spec start(inet:socket(), atom(), shaper:shaper()) -> undefined | pid().

%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Socket, SockMod, Shaper) ->
    start(Socket, SockMod, Shaper, infinity).

-spec start(inet:socket(), atom(), shaper:shaper(),
            non_neg_integer() | infinity) -> undefined | pid().

start(Socket, SockMod, Shaper, MaxStanzaSize) ->
    {ok, Pid} =
	supervisor:start_child(ejabberd_receiver_sup,
			       [Socket, SockMod, Shaper, MaxStanzaSize]),
    Pid.

-spec change_shaper(pid(), shaper:shaper()) -> ok.

change_shaper(Pid, Shaper) ->
    gen_server:cast(Pid, {change_shaper, Shaper}).

-spec reset_stream(pid()) -> ok | {error, any()}.

reset_stream(Pid) -> do_call(Pid, reset_stream).

-spec starttls(pid(), p1_tls:tls_socket()) -> ok.

<<<<<<< HEAD
starttls(Pid, TLSOpts) ->
    starttls(Pid, TLSOpts, undefined).

starttls(Pid, TLSOpts, Data) ->
    gen_server:call(Pid, {starttls, TLSOpts, Data}).

compress(Pid, Data) ->
    gen_server:call(Pid, {compress, Data}).
=======
starttls(Pid, TLSSocket) ->
    do_call(Pid, {starttls, TLSSocket}).

-spec compress(pid(), iodata() | undefined) -> {error, any()} |
                                               {ok, ezlib:zlib_socket()}.

compress(Pid, Data) ->
    do_call(Pid, {compress, Data}).

-spec become_controller(pid(), pid()) -> ok | {error, any()}.
>>>>>>> upstream/master

become_controller(Pid, C2SPid) ->
    do_call(Pid, {become_controller, C2SPid}).

-spec close(pid()) -> ok.

change_controller(Pid, C2SPid) ->
    gen_server:call(Pid, {change_controller, C2SPid}).

setopts(Pid, Opts) ->
    case lists:member({active, false}, Opts) of
	true ->
	    gen_server:call(Pid, deactivate_socket);
	false ->
	    ok
    end.

send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}).

close(Pid) ->
    gen_server:cast(Pid, close).


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
init([Socket, SockMod, Shaper, MaxStanzaSize]) ->
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
		ssl -> 20;
		_ -> infinity
	      end,
    {ok,
     #state{socket = Socket, sock_mod = SockMod,
	    shaper_state = ShaperState,
	    max_stanza_size = MaxStanzaSize, timeout = Timeout}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
<<<<<<< HEAD
handle_call({starttls, TLSOpts, Data}, _From, State) ->
    {ok, TLSSocket} = tls:tcp_to_tls(State#state.socket, TLSOpts),
    if Data /= undefined ->
	    do_send(State, Data);
       true ->
	    ok
    end,
    NewXMLStreamState = do_reset_stream(State),
=======
handle_call({starttls, TLSSocket}, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid,
		   max_stanza_size = MaxStanzaSize} = State) ->
    close_stream(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid,
				       MaxStanzaSize),
>>>>>>> upstream/master
    NewState = State#state{socket = TLSSocket,
			   sock_mod = p1_tls,
			   xml_stream_state = NewXMLStreamState},
    case p1_tls:recv_data(TLSSocket, <<"">>) of
	{ok, TLSData} ->
	    {NextState, Hib} = process_data(TLSData, NewState),
	    {reply, {ok, TLSSocket}, NextState, Hib};
	{error, _Reason} ->
	    {stop, normal, ok, NewState}
    end;
handle_call({compress, Data}, _From,
	    #state{xml_stream_state = XMLStreamState,
<<<<<<< HEAD
		   sock_mod = SockMod,
		   socket = Socket} = State) ->
    {ok, ZlibSocket} = ejabberd_zlib:enable_zlib(SockMod, Socket),
    if Data /= undefined ->
	    do_send(State, Data);
       true ->
	    ok
    end,
    NewXMLStreamState = exmpp_xmlstream:reset(XMLStreamState),
=======
		   c2s_pid = C2SPid, socket = Socket, sock_mod = SockMod,
		   max_stanza_size = MaxStanzaSize} =
		State) ->
    {ok, ZlibSocket} = ezlib:enable_zlib(SockMod,
						 Socket),
    if Data /= undefined -> do_send(State, Data);
       true -> ok
    end,
    close_stream(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid,
				       MaxStanzaSize),
>>>>>>> upstream/master
    NewState = State#state{socket = ZlibSocket,
			   sock_mod = ezlib,
			   xml_stream_state = NewXMLStreamState},
<<<<<<< HEAD
    case ejabberd_zlib:recv_data(ZlibSocket, "") of
	{ok, ZlibData} ->
	    {NextState, Hib} = process_data(ZlibData, NewState),
	    {reply, {ok, ZlibSocket}, NextState, Hib};
	{error, _Reason} ->
	    {stop, normal, ok, NewState}
    end;
handle_call(reset_stream, _From,
	    #state{xml_stream_state = XMLStreamState} = State) ->
    NewXMLStreamState = exmpp_xmlstream:reset(XMLStreamState),
=======
    case ezlib:recv_data(ZlibSocket, <<"">>) of
      {ok, ZlibData} ->
	  {reply, {ok, ZlibSocket},
	   process_data(ZlibData, NewState), ?HIBERNATE_TIMEOUT};
      {error, _Reason} -> {stop, normal, ok, NewState}
    end;
handle_call(reset_stream, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid, max_stanza_size = MaxStanzaSize} =
		State) ->
    close_stream(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid,
				       MaxStanzaSize),
>>>>>>> upstream/master
    Reply = ok,
    {reply, Reply,
     State#state{xml_stream_state = NewXMLStreamState},
     ?HIBERNATE_TIMEOUT};
handle_call({become_controller, C2SPid}, _From, State) ->
    erlang:monitor(process, C2SPid),
    close_stream(State#state.xml_stream_state),
    XMLStreamState = new_xmlstream(C2SPid, State#state.max_stanza_size),
    NewState = State#state{c2s_pid = C2SPid,
			   xml_stream_state = XMLStreamState},
    activate_socket(NewState),
    Reply = ok,
    {reply, Reply, NewState, ?HIBERNATE_TIMEOUT};
handle_call({change_controller, C2SPid}, _From, State) ->
    erlang:monitor(process, C2SPid),
    NewXMLStreamState = exmpp_xmlstream:change_callback(
			  State#state.xml_stream_state, {gen_fsm, C2SPid}),
    NewState = State#state{c2s_pid = C2SPid,
			   xml_stream_state = NewXMLStreamState},
    activate_socket(NewState),
    {reply, ok, NewState, ?HIBERNATE_TIMEOUT};
handle_call({send, Data}, _From, State) ->
    case do_send(State, Data) of
	ok ->
	    {reply, ok, State, ?HIBERNATE_TIMEOUT};
	{error, _Reason} = Err ->
	    {stop, normal, Err, State}
    end;
handle_call(deactivate_socket, _From, State) ->
    deactivate_socket(State),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = shaper:new(Shaper),
    {noreply, State#state{shaper_state = NewShaperState},
     ?HIBERNATE_TIMEOUT};
handle_cast(close, State) -> {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Tag, _TCPSocket, Data},
	    #state{socket = Socket, sock_mod = SockMod} = State)
    when (Tag == tcp) or (Tag == ssl) or
	   (Tag == ejabberd_xml) ->
    case SockMod of
<<<<<<< HEAD
	tls ->
	    case tls:recv_data(Socket, Data) of
		{ok, TLSData} ->
            {NextState, Hib} = process_data(TLSData, State),
		    {noreply, NextState, Hib};
		{error, _Reason} ->
		    {stop, normal, State}
	    end;
	ejabberd_zlib ->
	    case ejabberd_zlib:recv_data(Socket, Data) of
		{ok, ZlibData} ->
            {NextState, Hib} = process_data(ZlibData, State),
		    {noreply, NextState, Hib};
		{error, _Reason} ->
		    {stop, normal, State}
	    end;
	_ ->
        {NextState, Hib} = process_data(Data, State),
	    {noreply, NextState, Hib}
=======
      p1_tls ->
	  case p1_tls:recv_data(Socket, Data) of
	    {ok, TLSData} ->
		{noreply, process_data(TLSData, State),
		 ?HIBERNATE_TIMEOUT};
	    {error, _Reason} -> {stop, normal, State}
	  end;
      ezlib ->
	  case ezlib:recv_data(Socket, Data) of
	    {ok, ZlibData} ->
		{noreply, process_data(ZlibData, State),
		 ?HIBERNATE_TIMEOUT};
	    {error, _Reason} -> {stop, normal, State}
	  end;
      _ ->
	  {noreply, process_data(Data, State), ?HIBERNATE_TIMEOUT}
>>>>>>> upstream/master
    end;
handle_info({Tag, _TCPSocket}, State)
    when (Tag == tcp_closed) or (Tag == ssl_closed) ->
    {stop, normal, State};
handle_info({Tag, _TCPSocket, Reason}, State)
    when (Tag == tcp_error) or (Tag == ssl_error) ->
    case Reason of
      timeout -> {noreply, State, ?HIBERNATE_TIMEOUT};
      _ -> {stop, normal, State}
    end;
handle_info({'DOWN', _MRef, process, C2SPid, _},
	    #state{c2s_pid = C2SPid} = State) ->
    {stop, normal, State};
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(timeout, State) ->
<<<<<<< HEAD
    {noreply, State, hibernate};

=======
    proc_lib:hibernate(gen_server, enter_loop,
		       [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};
>>>>>>> upstream/master
handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason,
	  #state{xml_stream_state = XMLStreamState,
		 c2s_pid = C2SPid} =
	      State) ->
    close_stream(XMLStreamState),
    if C2SPid /= undefined ->
	   gen_fsm:send_event(C2SPid, closed);
       true -> ok
    end,
    catch (State#state.sock_mod):close(State#state.socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

activate_socket(#state{socket = Socket,
		       sock_mod = SockMod}) ->
    PeerName = case SockMod of
		 gen_tcp ->
		     inet:setopts(Socket, [{active, once}]),
		     inet:peername(Socket);
		 _ ->
		     SockMod:setopts(Socket, [{active, once}]),
		     SockMod:peername(Socket)
	       end,
    case PeerName of
      {error, _Reason} -> self() ! {tcp_closed, Socket};
      {ok, _} -> ok
    end.

deactivate_socket(#state{socket = Socket,
			 tref = TRef,
			 sock_mod = SockMod}) ->
    cancel_timer(TRef),
    case SockMod of
	gen_tcp ->
	    inet:setopts(Socket, [{active, false}]);
	_ ->
	    SockMod:setopts(Socket, [{active, false}])
    end.

%% Data processing for connectors directly generating xmlel in
%% Erlang data structure.
%% WARNING: Shaper does not work with Erlang data structure.
process_data([], State) ->
<<<<<<< HEAD
    activate_socket(State),
    State;
process_data([Element|Els], #state{c2s_pid = C2SPid} = State)
  when element(1, Element) == xmlel;
       element(1, Element) == xmlstreamstart;
      element(1, Element) == xmlstreamelement;
       element(1, Element) == xmlstreamend ->
    if
	C2SPid == undefined ->
	    State;
	true ->
	    catch gen_fsm:send_event(C2SPid, element_wrapper(Element)),
	    process_data(Els, State)
=======
    activate_socket(State), State;
process_data([Element | Els],
	     #state{c2s_pid = C2SPid} = State)
    when element(1, Element) == xmlel;
	 element(1, Element) == xmlstreamstart;
	 element(1, Element) == xmlstreamelement;
	 element(1, Element) == xmlstreamend ->
    if C2SPid == undefined -> State;
       true ->
	   catch gen_fsm:send_event(C2SPid,
				    element_wrapper(Element)),
	   process_data(Els, State)
>>>>>>> upstream/master
    end;
%% Data processing for connectors receivind data as string.
process_data(Data,
	     #state{xml_stream_state = XMLStreamState,
<<<<<<< HEAD
		    tref = TRef,
		    shaper_state = ShaperState,
		    c2s_pid = C2SPid} = State) ->
    ?DEBUG("Received XML on stream = ~p", [Data]),
    {ok, XMLStreamState1} = exmpp_xmlstream:parse(XMLStreamState, Data),
    {NewShaperState, Pause} = shaper:update(ShaperState, size(Data)),
    {NewTRef, HibTimeout} = 
        if
	    C2SPid == undefined ->
		{TRef, infinity};
        Pause > 0 ->
		{erlang:start_timer(Pause, self(), activate), hibernate};
	    true ->
		activate_socket(State),
		{TRef, ?HIBERNATE_TIMEOUT}
        end,
    {State#state{xml_stream_state = XMLStreamState1,
		 tref = NewTRef,
		 shaper_state = NewShaperState}, HibTimeout}.
=======
		    shaper_state = ShaperState, c2s_pid = C2SPid} =
		 State) ->
    ?DEBUG("Received XML on stream = ~p", [(Data)]),
    XMLStreamState1 = xml_stream:parse(XMLStreamState, Data),
    {NewShaperState, Pause} = shaper:update(ShaperState, byte_size(Data)),
    if
	C2SPid == undefined ->
	    ok;
	Pause > 0 ->
	    erlang:start_timer(Pause, self(), activate);
	true ->
	    activate_socket(State)
    end,
    State#state{xml_stream_state = XMLStreamState1,
		shaper_state = NewShaperState}.
>>>>>>> upstream/master

%% Element coming from XML parser are wrapped inside xmlstreamelement
%% When we receive directly xmlel tuple (from a socket module
%% speaking directly Erlang XML), we wrap it inside the same
%% xmlstreamelement coming from the XML parser.
<<<<<<< HEAD
element_wrapper(XmlEl)
  when element(1, XmlEl) == xmlel ->
    {xmlstreamelement, XmlEl};
element_wrapper(Element) ->
    Element.
=======
element_wrapper(XMLElement)
    when element(1, XMLElement) == xmlel ->
    {xmlstreamelement, XMLElement};
element_wrapper(Element) -> Element.
>>>>>>> upstream/master

close_stream(undefined) -> ok;
close_stream(XMLStreamState) ->
<<<<<<< HEAD
    exmpp_xml:stop_parser(exmpp_xmlstream:get_parser(XMLStreamState)),
    exmpp_xmlstream:stop(XMLStreamState).
    
do_send(State, Data) ->
    (State#state.sock_mod):send(State#state.socket, Data).

cancel_timer(TRef) when is_reference(TRef) ->
    case erlang:cancel_timer(TRef) of
	false ->
	    receive
		{timeout, TRef, _} ->
		    ok
	    after 0 ->
		    ok
	    end;
	_ ->
	    ok
    end;
cancel_timer(_) ->
    ok.

do_reset_stream(#state{xml_stream_state = undefined, c2s_pid = C2SPid, max_stanza_size = MaxStanzaSize}) ->
    new_xmlstream(C2SPid, MaxStanzaSize);

do_reset_stream(#state{xml_stream_state = XMLStreamState}) ->
    exmpp_xmlstream:reset(XMLStreamState).


new_xmlstream(C2SPid, MaxStanzaSize) ->
    Parser = exmpp_xml:start_parser([{names_as_atom, true},
                                     {check_nss, xmpp},
                                     {check_elems, xmpp},
                                     {max_size, MaxStanzaSize}
                                    ]),
    exmpp_xmlstream:start({gen_fsm, C2SPid}, Parser,
                          [{xmlstreamstart, new}]
                         ).
=======
    xml_stream:close(XMLStreamState).

do_send(State, Data) ->
    (State#state.sock_mod):send(State#state.socket, Data).

do_call(Pid, Msg) ->
    case catch gen_server:call(Pid, Msg) of
      {'EXIT', Why} -> {error, Why};
      Res -> Res
    end.
>>>>>>> upstream/master
