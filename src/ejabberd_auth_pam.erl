%%%-------------------------------------------------------------------
%%% File    : ejabberd_auth_pam.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : PAM authentication
%%% Created : 5 Jul 2007 by Evgeniy Khramtsov <xram@jabber.ru>
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
%%%-------------------------------------------------------------------
-module(ejabberd_auth_pam).

-author('xram@jabber.ru').

<<<<<<< HEAD
%% External exports
-export([start/1,
	 stop/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 store_type/0,
	 plain_password_required/0
	]).
=======
-behaviour(ejabberd_auth).
>>>>>>> upstream/master

%% External exports
%%====================================================================
%% API
%%====================================================================
<<<<<<< HEAD

%% @spec (Host) -> ok | term()
%%     Host = string()
=======
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
         get_vh_registered_users/2, get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
	 get_password/2, get_password_s/2, is_user_exists/2,
	 remove_user/2, remove_user/3, store_type/0,
	 plain_password_required/0]).
>>>>>>> upstream/master

start(_Host) ->
    ejabberd:start_app(p1_pam).

%% TODO: Stop epam if no other auth_pam are running.
stop(_Host) ->
    ok.

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()

set_password(_User, _Server, _Password) ->
    {error, not_allowed}.

<<<<<<< HEAD
%% @spec (User, Server, Password, Digest, DigestGen) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Digest = string()
%%     DigestGen = function()

check_password(User, Server, Password, _StreamID, _Digest) ->
    check_password(User, Server, Password).

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password(User, Server, Password) ->
    Service = get_pam_service(Server),
    UserInfo = case get_pam_userinfotype(Server) of
	username -> User;
	jid -> User++"@"++Server
    end,
    case catch epam:authenticate(Service, UserInfo, Password) of
	true -> true;
	_    -> false
=======
check_password(User, Server, Password, _Digest,
	       _DigestGen) ->
    check_password(User, Server, Password).

check_password(User, Host, Password) ->
    Service = get_pam_service(Host),
    UserInfo = case get_pam_userinfotype(Host) of
		 username -> User;
		 jid -> <<User/binary, "@", Host/binary>>
	       end,
    case catch epam:authenticate(Service, UserInfo,
				 Password)
	of
      true -> true;
      _ -> false
>>>>>>> upstream/master
    end.

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

<<<<<<< HEAD
%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()

dirty_get_registered_users() ->
    [].

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(_Server) ->
    [].

%% @spec (User, Server) -> Password | false
%%     User = string()
%%     Server = string()
%%     Password = string()

get_password(_User, _Server) ->
    false.

%% @spec (User, Server) -> Password | nil()
%%     User = string()
%%     Server = string()
%%     Password = string()

get_password_s(_User, _Server) ->
    "".

%% @spec (User, Server) -> true | false | {error, Error}
%%     User = string()
%%     Server = string()
%% TODO: Improve this function to return an error instead of 'false' when
%% connection to PAM failed

is_user_exists(User, Server) ->
    Service = get_pam_service(Server),
    UserInfo = case get_pam_userinfotype(Server) of
	username -> User;
	jid -> User++"@"++Server
    end,
    case catch epam:acct_mgmt(Service, UserInfo) of
	true -> true;
	_    -> false
    end.

%% @spec (User, Server) -> {error, not_allowed}
%%     User = string()
%%     Server = string()

remove_user(_User, _Server) ->
    {error, not_allowed}.

%% @spec (User, Server, Password) -> not_allowed
%%     User = string()
%%     Server = string()
%%     Password = string()

remove_user(_User, _Server, _Password) ->
    not_allowed.

%% @spec () -> bool()

plain_password_required() ->
    true.
=======
dirty_get_registered_users() -> [].

get_vh_registered_users(_Host) -> [].

get_vh_registered_users(_Host, _) -> [].

get_vh_registered_users_number(_Host) -> 0.

get_vh_registered_users_number(_Host, _) -> 0.

get_password(_User, _Server) -> false.

get_password_s(_User, _Server) -> <<"">>.

%% @spec (User, Server) -> true | false | {error, Error}
%% TODO: Improve this function to return an error instead of 'false' when connection to PAM failed
is_user_exists(User, Host) ->
    Service = get_pam_service(Host),
    UserInfo = case get_pam_userinfotype(Host) of
		 username -> User;
		 jid -> <<User/binary, "@", Host/binary>>
	       end,
    case catch epam:acct_mgmt(Service, UserInfo) of
      true -> true;
      _ -> false
    end.

remove_user(_User, _Server) -> {error, not_allowed}.

remove_user(_User, _Server, _Password) -> not_allowed.

plain_password_required() -> true.

store_type() -> external.
>>>>>>> upstream/master

store_type() ->
	external.

%%====================================================================
%% Internal functions
%%====================================================================
<<<<<<< HEAD

%% @spec (Server) -> string()
%%     Server = string()

get_pam_service(Server) ->
    case ejabberd_config:get_local_option({pam_service, Server}) of
	undefined -> "ejabberd";
	Service   -> Service
    end.
get_pam_userinfotype(Host) ->
    case ejabberd_config:get_local_option({pam_userinfotype, Host}) of
	undefined -> username;
	Type -> Type
    end.
=======
get_pam_service(Host) ->
    ejabberd_config:get_option(
      {pam_service, Host},
      fun iolist_to_binary/1,
      <<"ejabberd">>).

get_pam_userinfotype(Host) ->
    ejabberd_config:get_option(
      {pam_userinfotype, Host},
      fun(username) -> username;
         (jid) -> jid
      end,
      username).
>>>>>>> upstream/master
