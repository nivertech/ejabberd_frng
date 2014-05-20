%%%----------------------------------------------------------------------
%%% File    : cyrsasl_anonymous.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : ANONYMOUS SASL mechanism
%%%  See http://www.ietf.org/internet-drafts/draft-ietf-sasl-anon-05.txt
%%% Created : 23 Aug 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
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

-module(cyrsasl_anonymous).

-export([start/1, stop/0, mech_new/1, mech_step/2]).

-include("cyrsasl.hrl").

-behaviour(cyrsasl).

<<<<<<< HEAD
%% @type mechstate() = {state, Server}
%%     Server = string().

-record(state, {server}).
=======
-record(state, {server = <<"">> :: binary()}).
>>>>>>> upstream/master

%% @spec (Opts) -> true
%%     Opts = term()

start(_Opts) ->
<<<<<<< HEAD
    cyrsasl:register_mechanism("ANONYMOUS", ?MODULE, plain),
    ok.

%% @spec () -> ok

stop() ->
    ok.
=======
    cyrsasl:register_mechanism(<<"ANONYMOUS">>, ?MODULE, plain),
    ok.

stop() -> ok.
>>>>>>> upstream/master

mech_new(#sasl_params{host=Host}) ->
    {ok, #state{server = Host}}.

<<<<<<< HEAD
%% @spec (State, ClientIn) -> Ok | Error
%%     State = mechstate()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = {username, Username} | {auth_module, AuthModule}
%%         Username = string()
%%         AuthModule = ejabberd_auth_anonymous
%%     Error = {error, 'not-authorized'}

mech_step(State, _ClientIn) ->
    %% We generate a random username:
    User = lists:concat([randoms:get_string() | tuple_to_list(now())]),
    Server = State#state.server,
    
    %% Checks that the username is available
    case ejabberd_auth:is_user_exists(User, Server) of
	true  -> {error, 'not-authorized'};
	false -> {ok, [{username, User},
		       {auth_module, ejabberd_auth_anonymous}]}
=======
mech_step(#state{server = Server}, _ClientIn) ->
    User = iolist_to_binary([randoms:get_string()
			     | [jlib:integer_to_binary(X)
				|| X <- tuple_to_list(now())]]),
    case ejabberd_auth:is_user_exists(User, Server) of
        true  -> {error, <<"not-authorized">>};
        false -> {ok, [{username, User}, {auth_module, ejabberd_auth_anonymous}]}
>>>>>>> upstream/master
    end.
