%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_nopwd.erl
%%% Author  : Zvi Avraham <zvi@niveretch.com>
%%% Purpose : Authentification - ignore poassword
%%% Created : 25 Jan 2011
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_nopwd).
-author('zvi@niveretch.com').

%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

%-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    ok.

plain_password_required() ->
    false.

check_password(_User, _Server, _Password) ->
    %LUser = jlib:nodeprep(_User),
    %LServer = jlib:nameprep(_Server),
    %if (LUser == error) or (LServer == error) ->
	%       {error, invalid_jid};
	%   true ->
    true.

check_password(_User, _Server, _Password, _Digest, _DigestGen) ->
    true.

%% @spec (_User::string(), _Server::string(), _Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(_User, _Server, _Password) ->
	ok.

%% @spec (_User, _Server, _Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
try_register(_User, _Server, _Password) ->
    {atomic, ok}.
 
%% Get all registered users
dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Server) ->
    [].

get_vh_registered_users(_Server, _) ->
    [].

get_vh_registered_users_number(_Server) ->
    0.

get_vh_registered_users_number(_Server, _) ->
    0.

get_password(_User, _Server) ->
    false.

get_password_s(_User, _Server) ->
    "".

%% @spec (_User, _Server) -> true | false | {error, Error}
is_user_exists(_User, _Server) ->
	true.

%% @spec (_User, _Server) -> ok
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
remove_user(_User, _Server) ->
	ok.

%% @spec (_User, _Server, _Password) -> ok | not_exists | not_allowed | bad_request
%% @doc Remove user if the provided password is correct.
remove_user(_User, _Server, _Password) ->
	ok.

