%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

%% TODO: Use the functions in ejabberd auth to add and remove users.

-module(ejabberd_auth).

-author('alexey@process-one.net').

%% External exports
<<<<<<< HEAD
-export([start/0,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 check_password_with_authmodule/3,
	 check_password_with_authmodule/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 get_password_with_authmodule/2,
	 is_user_exists/2,
	 is_user_exists_in_other_modules/3,
	 remove_user/2,
	 remove_user/3,
	 remove_host/1,
	 plain_password_required/1,
	 store_type/1,
	 entropy/1
	]).
=======
-export([start/0, set_password/3, check_password/3,
	 check_password/5, check_password_with_authmodule/3,
	 check_password_with_authmodule/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2, export/1, import/1,
	 get_vh_registered_users_number/1, import/3,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, get_password_with_authmodule/2,
	 is_user_exists/2, is_user_exists_in_other_modules/3,
	 remove_user/2, remove_user/3, plain_password_required/1,
	 store_type/1, entropy/1]).
>>>>>>> upstream/master

-export([start/1
         ,stop/1
         ,start_modules/2
         ,stop_modules/2
        ]).

-export([auth_modules/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%% @type authmodule() = ejabberd_auth_anonymous | ejabberd_auth_external |
%%                      ejabberd_auth_ldap | ejabberd_auth_pam |
%%                      ejabberd_auth_storage | atom().

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
<<<<<<< HEAD

%% @spec () -> term()

start() ->
    ?DEBUG("About to start auth modules. Hosts: ~p", [?MYHOSTS]),
    lists:foreach(fun start/1, ?MYHOSTS).

start(Host) ->
    ejabberd_hooks:add(remove_host, list_to_binary(Host), ?MODULE, remove_host, 150),
    start_modules(Host, auth_modules(Host)).

stop(Host) ->
    ejabberd_hooks:delete(remove_host, list_to_binary(Host), ?MODULE, remove_host, 150),
    stop_modules(Host, auth_modules(Host)).

remove_host(HostB) when is_binary(HostB) ->
    lists:foreach(
        fun({Username, Host}) ->
            ejabberd_auth:remove_user(Username, Host)
        end,
        ejabberd_auth:get_vh_registered_users(binary_to_list(HostB))).

start_modules(Host, Modules) when is_list(Modules) ->
    lists:foreach(fun (M) -> start_modules(Host, M) end, Modules);
start_modules(Host, Module) when is_atom(Module) ->
    Module:start(Host).

stop_modules(Host, Modules) when is_list(Modules) ->
    lists:foreach(fun (M) -> stop_modules(Host, M) end, Modules);
stop_modules(Host, Module) when is_atom(Module) ->
    Module:stop(Host).

%% @spec (Server) -> bool()
%%     Server = string()

%% This is only executed by ejabberd_c2s for non-SASL auth client
plain_password_required(Server) when is_list(Server) ->
    lists:any(
      fun(M) ->
	      M:plain_password_required()
      end, auth_modules(Server)).

%% @spec (Server) -> bool()
%%     Server = string()

store_type(Server) ->
    lists:foldl(
      fun(_, external) ->
	      external;
	  (M, scram) ->
	      case M:store_type() of
		  external ->
		      external;
		  _Else ->
		      scram
		  end;
	  (M, plain) ->
	      M:store_type()
      end, plain, auth_modules(Server)).

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Check if the user and password can login in server.

check_password(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    case check_password_with_authmodule(User, Server, Password) of
	{true, _AuthModule} -> true;
	{false, _Reason} -> false
=======
-type opts() :: [{prefix, binary()} | {from, integer()} |
                 {to, integer()} | {limit, integer()} |
                 {offset, integer()}].

-callback start(binary()) -> any().
-callback plain_password_required() -> boolean().
-callback store_type() -> plain | external | scram.
-callback set_password(binary(), binary(), binary()) -> ok | {error, atom()}.
-callback remove_user(binary(), binary()) -> any().
-callback remove_user(binary(), binary(), binary()) -> any().
-callback is_user_exists(binary(), binary()) -> boolean() | {error, atom()}.
-callback check_password(binary(), binary(), binary()) -> boolean().
-callback check_password(binary(), binary(), binary(), binary(),
                         fun((binary()) -> binary())) -> boolean().
-callback try_register(binary(), binary(), binary()) -> {atomic, atom()} |
                                                        {error, atom()}.
-callback dirty_get_registered_users() -> [{binary(), binary()}].
-callback get_vh_registered_users(binary()) -> [{binary(), binary()}].
-callback get_vh_registered_users(binary(), opts()) -> [{binary(), binary()}].
-callback get_vh_registered_users_number(binary()) -> number().
-callback get_vh_registered_users_number(binary(), opts()) -> number().
-callback get_password(binary(), binary()) -> false | binary().
-callback get_password_s(binary(), binary()) -> binary().    

start() ->
%% This is only executed by ejabberd_c2s for non-SASL auth client
    lists:foreach(fun (Host) ->
			  lists:foreach(fun (M) -> M:start(Host) end,
					auth_modules(Host))
		  end,
		  ?MYHOSTS).

plain_password_required(Server) ->
    lists:any(fun (M) -> M:plain_password_required() end,
	      auth_modules(Server)).

store_type(Server) ->
%% @doc Check if the user and password can login in server.
%% @spec (User::string(), Server::string(), Password::string()) ->
%%     true | false
    lists:foldl(fun (_, external) -> external;
		    (M, scram) ->
			case M:store_type() of
			  external -> external;
			  _Else -> scram
			end;
		    (M, plain) -> M:store_type()
		end,
		plain, auth_modules(Server)).

-spec check_password(binary(), binary(), binary()) -> boolean().

check_password(User, Server, Password) ->
    case check_password_with_authmodule(User, Server,
					Password)
	of
      {true, _AuthModule} -> true;
      false -> false
>>>>>>> upstream/master
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Digest = string()
%%     DigestGen = function()
%% @doc Check if the user and password can login in server.
<<<<<<< HEAD

check_password(User, Server, Password, Digest, DigestGen)
  when is_list(User), is_list(Server), is_list(Password),
  is_list(Digest), is_function(DigestGen) ->
    case check_password_with_authmodule(User, Server, Password,
					Digest, DigestGen) of
	{true, _AuthModule} -> true;
	{false, _Reason} -> false
=======
%% @spec (User::string(), Server::string(), Password::string(),
%%        Digest::string(), DigestGen::function()) ->
%%     true | false
-spec check_password(binary(), binary(), binary(), binary(),
                     fun((binary()) -> binary())) -> boolean().
                                 
check_password(User, Server, Password, Digest,
	       DigestGen) ->
    case check_password_with_authmodule(User, Server,
					Password, Digest, DigestGen)
	of
      {true, _AuthModule} -> true;
      false -> false
>>>>>>> upstream/master
    end.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%     {true, AuthModule} | {false, Reason::string()}
%% where
%%   AuthModule = ejabberd_auth_anonymous | ejabberd_auth_external
%%                 | ejabberd_auth_internal | ejabberd_auth_ldap
%%                 | ejabberd_auth_odbc | ejabberd_auth_pam
<<<<<<< HEAD
%% @doc Check if the user and password can login in server.
%% The user can login if at least an authentication method accepts the user
%% and the password.
%% The first authentication method that accepts the credentials is returned.
check_password_with_authmodule(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    check_password_loop(auth_modules(Server), [User, Server, Password], "").

%% @spec (User, Server, Password, Digest, DigestGen) -> {true, AuthModule} | false
%%     User = string()
%%     Server = string()
%%     Password = string() | undefined
%%     Digest = string() | undefined
%%     DigestGen = function()
%%     AuthModule = authmodule()
%% @doc Check the password is valid and also return the authentication module that accepts it.
%% The password is 'undefined' if the client
%% authenticates using the digest method as defined in
%% XEP-0078: Non-SASL Authentication

check_password_with_authmodule(User, Server, Password, Digest, DigestGen)
  when is_list(User), is_list(Server), (is_list(Password) orelse Password == 'undefined'),
  is_function(DigestGen), (is_list(Digest) orelse Digest == 'undefined')->
    check_password_loop(auth_modules(Server), [User, Server, Password,
					       Digest, DigestGen], "").

check_password_loop([], _Args, LastReason) ->
    {false, LastReason};
check_password_loop([AuthModule | AuthModules], Args, PreviousReason) ->
    case apply(AuthModule, check_password, Args) of
	true ->
	    {true, AuthModule};
	{false, Reason} when Reason /= "" ->
	    check_password_loop(AuthModules, Args, Reason);
	{false, ""} ->
	    check_password_loop(AuthModules, Args, PreviousReason);
	false ->
	    check_password_loop(AuthModules, Args, PreviousReason)
    end.

%% @spec (User, Server, Password) -> ok | {error, ErrorType}
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     ErrorType = empty_password | not_allowed | invalid_jid
set_password(_User, _Server, "") ->
    %% We do not allow empty password
    {error, empty_password};
set_password(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    lists:foldl(
      fun(M, {error, _}) ->
	      M:set_password(User, Server, Password);
	 (_M, Res) ->
	      Res
      end, {error, not_allowed}, auth_modules(Server)).

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string() | nil()
try_register(_User, _Server, "") ->
    %% We do not allow empty password
    {error, not_allowed};
try_register(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    case is_user_exists(User, Server) of
	true ->
	    {atomic, exists};
	false ->
	    case ?IS_MY_HOST(exmpp_stringprep:nameprep(Server)) of
		true ->
		    Res = lists:foldl(
			    fun (_M, {atomic, ok} = Res) ->
				    Res;
				(M, _) ->
				    M:try_register(User, Server, Password)
			    end, {error, not_allowed}, auth_modules(Server)),
		    trigger_register_hooks(Res, User, Server);
		false ->
		    {error, not_allowed}
	    end
    end.

trigger_register_hooks({atomic, ok} = Res, User, Server) ->
    ejabberd_hooks:run(register_user, list_to_binary(Server),
		       [User, Server]),
    Res;
trigger_register_hooks(Res, _User, _Server) ->
    Res.

%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()
%% @doc Registered users list do not include anonymous users logged.
=======
-spec check_password_with_authmodule(binary(), binary(), binary()) -> false |
                                                                      {true, atom()}.

check_password_with_authmodule(User, Server,
			       Password) ->
    check_password_loop(auth_modules(Server),
			[User, Server, Password]).

-spec check_password_with_authmodule(binary(), binary(), binary(), binary(),
                                     fun((binary()) -> binary())) -> false |
                                                                     {true, atom()}.

check_password_with_authmodule(User, Server, Password,
			       Digest, DigestGen) ->
    check_password_loop(auth_modules(Server),
			[User, Server, Password, Digest, DigestGen]).

check_password_loop([], _Args) -> false;
check_password_loop([AuthModule | AuthModules], Args) ->
    case apply(AuthModule, check_password, Args) of
      true -> {true, AuthModule};
      false -> check_password_loop(AuthModules, Args)
    end.

-spec set_password(binary(), binary(), binary()) -> ok |
                                                    {error, atom()}.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, ErrorType}
%% where ErrorType = empty_password | not_allowed | invalid_jid
set_password(_User, _Server, <<"">>) ->
    {error, empty_password};
set_password(User, Server, Password) ->
%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, not_allowed}
    lists:foldl(fun (M, {error, _}) ->
			M:set_password(User, Server, Password);
		    (_M, Res) -> Res
		end,
		{error, not_allowed}, auth_modules(Server)).

-spec try_register(binary(), binary(), binary()) -> {atomic, atom()} |
                                                    {error, atom()}.

try_register(_User, _Server, <<"">>) ->
    {error, not_allowed};
try_register(User, Server, Password) ->
    case is_user_exists(User, Server) of
      true -> {atomic, exists};
      false ->
	  case lists:member(jlib:nameprep(Server), ?MYHOSTS) of
	    true ->
		Res = lists:foldl(fun (_M, {atomic, ok} = Res) -> Res;
				      (M, _) ->
					  M:try_register(User, Server, Password)
				  end,
				  {error, not_allowed}, auth_modules(Server)),
		case Res of
		  {atomic, ok} ->
		      ejabberd_hooks:run(register_user, Server,
					 [User, Server]),
		      {atomic, ok};
		  _ -> Res
		end;
	    false -> {error, not_allowed}
	  end
    end.

%% Registered users list do not include anonymous users logged
-spec dirty_get_registered_users() -> [{binary(), binary()}].
>>>>>>> upstream/master

dirty_get_registered_users() ->
    lists:flatmap(fun (M) -> M:dirty_get_registered_users()
		  end,
		  auth_modules()).

-spec get_vh_registered_users(binary()) -> [{binary(), binary()}].

<<<<<<< HEAD
%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()
%% @doc Registered users list do not include anonymous users logged.

get_vh_registered_users(Server) when is_list(Server) ->
    lists:flatmap(
      fun(M) ->
	      M:get_vh_registered_users(Server)
      end, auth_modules(Server)).

%% @spec (Server, Opts) -> [{LUser, LServer}]
%%     Server = string()
%%     Opts = [{Opt, Val}]
%%         Opt = atom()
%%         Val = term()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server, Opts) when is_list(Server) ->
    lists:flatmap(
      fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users, 2) of
		    true ->
			M:get_vh_registered_users(Server, Opts);
		    false ->
			M:get_vh_registered_users(Server)
		end
      end, auth_modules(Server)).

%% @spec (Server) -> Users_Number
%%     Server = string()
%%     Users_Number = integer()

get_vh_registered_users_number(Server) when is_list(Server) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 1) of
		    true ->
			M:get_vh_registered_users_number(Server);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

%% @spec (Server, Opts) -> Users_Number
%%     Server = string()
%%     Opts = [{Opt, Val}]
%%         Opt = atom()
%%         Val = term()
%%     Users_Number = integer()

get_vh_registered_users_number(Server, Opts) when is_list(Server) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 2) of
		    true ->
			M:get_vh_registered_users_number(Server, Opts);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

%% @spec (User, Server) -> Password | false
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Get the password of the user.

get_password(User, Server) when is_list(User), is_list(Server) ->
    lists:foldl(
      fun(M, false) ->
	      M:get_password(User, Server);
	 (_M, Password) ->
	      Password
      end, false, auth_modules(Server)).
=======
%% Registered users list do not include anonymous users logged
get_vh_registered_users(Server) ->
    lists:flatmap(fun (M) ->
			  M:get_vh_registered_users(Server)
		  end,
		  auth_modules(Server)).

-spec get_vh_registered_users(binary(), opts()) -> [{binary(), binary()}].

get_vh_registered_users(Server, Opts) ->
    lists:flatmap(fun (M) ->
			  case erlang:function_exported(M,
							get_vh_registered_users,
							2)
			      of
			    true -> M:get_vh_registered_users(Server, Opts);
			    false -> M:get_vh_registered_users(Server)
			  end
		  end,
		  auth_modules(Server)).

get_vh_registered_users_number(Server) ->
    lists:sum(lists:map(fun (M) ->
				case erlang:function_exported(M,
							      get_vh_registered_users_number,
							      1)
				    of
				  true ->
				      M:get_vh_registered_users_number(Server);
				  false ->
				      length(M:get_vh_registered_users(Server))
				end
			end,
			auth_modules(Server))).

-spec get_vh_registered_users_number(binary(), opts()) -> number().

get_vh_registered_users_number(Server, Opts) ->
%% @doc Get the password of the user.
%% @spec (User::string(), Server::string()) -> Password::string()
    lists:sum(lists:map(fun (M) ->
				case erlang:function_exported(M,
							      get_vh_registered_users_number,
							      2)
				    of
				  true ->
				      M:get_vh_registered_users_number(Server,
								       Opts);
				  false ->
				      length(M:get_vh_registered_users(Server))
				end
			end,
			auth_modules(Server))).

-spec get_password(binary(), binary()) -> false | binary().

get_password(User, Server) ->
    lists:foldl(fun (M, false) ->
			M:get_password(User, Server);
		    (_M, Password) -> Password
		end,
		false, auth_modules(Server)).

-spec get_password_s(binary(), binary()) -> binary().
>>>>>>> upstream/master

%% @spec (User, Server) -> Password | nil()
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Get the password of the user.

get_password_s(User, Server) when is_list(User), is_list(Server) ->
    case get_password(User, Server) of
<<<<<<< HEAD
	false ->
	    "";
	Password when is_list(Password) ->
	    Password;
	_ ->
	    ""
=======
      false -> <<"">>;
      Password -> Password
>>>>>>> upstream/master
    end.

%% @spec (User, Server) -> {Password, AuthModule} | {false, none}
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     AuthModule = authmodule()
%% @doc Get the password of the user and the auth module.
<<<<<<< HEAD

get_password_with_authmodule(User, Server)
  when is_list(User), is_list(Server) ->
    lists:foldl(
      fun(M, {false, _}) ->
	      {M:get_password(User, Server), M};
	 (_M, {Password, AuthModule}) ->
	      {Password, AuthModule}
      end, {false, none}, auth_modules(Server)).

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()
%% @doc Returns true if the user exists in the DB or if an anonymous
%% user is logged under the given name.

is_user_exists(User, Server) when is_list(User), is_list(Server) ->
    lists:any(
      fun(M) ->
	      case M:is_user_exists(User, Server) of
		  {error, Error} ->
		      ?ERROR_MSG("The authentication module ~p returned an "
				 "error~nwhen checking user ~p in server ~p~n"
				 "Error message: ~p",
				 [M, User, Server, Error]),
		      false;
		  Else ->
		      Else
	      end
      end, auth_modules(Server)).

%% @spec (Module, User, Server) -> true | false | maybe
%%     Module = authmodule()
%%     User = string()
%%     Server = string()
%% @doc Check if the user exists in all authentications module except
%% the module passed as parameter.

is_user_exists_in_other_modules(Module, User, Server)
  when is_list(User), is_list(Server) ->
    is_user_exists_in_other_modules_loop(
      auth_modules(Server)--[Module],
      User, Server).
is_user_exists_in_other_modules_loop([], _User, _Server) ->
=======
%% @spec (User::string(), Server::string()) ->
%%     {Password::string(), AuthModule::atom()} | {false, none}
-spec get_password_with_authmodule(binary(), binary()) -> {false | binary(), atom()}.

get_password_with_authmodule(User, Server) ->
%% Returns true if the user exists in the DB or if an anonymous user is logged
%% under the given name
    lists:foldl(fun (M, {false, _}) ->
			{M:get_password(User, Server), M};
		    (_M, {Password, AuthModule}) -> {Password, AuthModule}
		end,
		{false, none}, auth_modules(Server)).

-spec is_user_exists(binary(), binary()) -> boolean().

is_user_exists(User, <<"">>) ->
    false;

is_user_exists(User, Server) ->
%% Check if the user exists in all authentications module except the module
%% passed as parameter
%% @spec (Module::atom(), User, Server) -> true | false | maybe
    lists:any(fun (M) ->
		      case M:is_user_exists(User, Server) of
			{error, Error} ->
			    ?ERROR_MSG("The authentication module ~p returned "
				       "an error~nwhen checking user ~p in server "
				       "~p~nError message: ~p",
				       [M, User, Server, Error]),
			    false;
			Else -> Else
		      end
	      end,
	      auth_modules(Server)).

-spec is_user_exists_in_other_modules(atom(), binary(), binary()) -> boolean() | maybe.

is_user_exists_in_other_modules(Module, User, Server) ->
    is_user_exists_in_other_modules_loop(auth_modules(Server)
					   -- [Module],
					 User, Server).

is_user_exists_in_other_modules_loop([], _User,
				     _Server) ->
>>>>>>> upstream/master
    false;
is_user_exists_in_other_modules_loop([AuthModule
				      | AuthModules],
				     User, Server) ->
    case AuthModule:is_user_exists(User, Server) of
      true -> true;
      false ->
	  is_user_exists_in_other_modules_loop(AuthModules, User,
					       Server);
      {error, Error} ->
	  ?DEBUG("The authentication module ~p returned "
		 "an error~nwhen checking user ~p in server "
		 "~p~nError message: ~p",
		 [AuthModule, User, Server, Error]),
	  maybe
    end.

-spec remove_user(binary(), binary()) -> ok.

<<<<<<< HEAD
%% @spec (User, Server) -> ok | error | {error, not_allowed}
%%     User = string()
%%     Server = string()
%% @doc Remove user.
%% TODO: Fix me: It always return ok even if there was some problem removing the user.
%% dialyzer warning
%% ejabberd_auth.erl:388: The variable _ can never match since previous clauses completely covered the type 'ok'

remove_user(User, Server) when is_list(User), is_list(Server) ->
    R = lists:foreach(
	  fun(M) ->
		  M:remove_user(User, Server)
	  end, auth_modules(Server)),
    ejabberd_hooks:run(remove_user, list_to_binary(exmpp_stringprep:nameprep(Server)),
				 [list_to_binary(User), list_to_binary(Server)]),
    R.
=======
%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(User, Server) ->
    lists:foreach(fun (M) -> M:remove_user(User, Server)
		  end,
		  auth_modules(Server)),
    ejabberd_hooks:run(remove_user, jlib:nameprep(Server),
		       [User, Server]),
    ok.
>>>>>>> upstream/master

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request | error
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Try to remove user if the provided password is correct.
%% The removal is attempted in each auth method provided:
%% when one returns 'ok' the loop stops;
%% if no method returns 'ok' then it returns the error message indicated by the last method attempted.
<<<<<<< HEAD

remove_user(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    R = lists:foldl(
      fun(_M, ok = Res) ->
	      Res;
	 (M, _) ->
	      M:remove_user(User, Server, Password)
      end, error, auth_modules(Server)),
    case R of
		ok -> ejabberd_hooks:run(remove_user, list_to_binary(exmpp_stringprep:nameprep(Server)),
                                    [list_to_binary(User), list_to_binary(Server)]);
		_ -> none
=======
-spec remove_user(binary(), binary(), binary()) -> any().

remove_user(User, Server, Password) ->
    R = lists:foldl(fun (_M, ok = Res) -> Res;
			(M, _) -> M:remove_user(User, Server, Password)
		    end,
		    error, auth_modules(Server)),
    case R of
      ok ->
	  ejabberd_hooks:run(remove_user, jlib:nameprep(Server),
			     [User, Server]);
      _ -> none
>>>>>>> upstream/master
    end,
    R.

%% @spec (IOList) -> non_negative_float()
%% @doc Calculate informational entropy.
<<<<<<< HEAD
entropy(IOList) ->
    case binary_to_list(iolist_to_binary(IOList)) of
	"" ->
	    0.0;
	S ->
	    Set = lists:foldl(
		    fun(C, [Digit, Printable, LowLetter, HiLetter, Other]) ->
			    if C >= $a, C =< $z ->
				    [Digit, Printable, 26, HiLetter, Other];
			       C >= $0, C =< $9 ->
				    [9, Printable, LowLetter, HiLetter, Other];
			       C >= $A, C =< $Z ->
				    [Digit, Printable, LowLetter, 26, Other];
			       C >= 16#21, C =< 16#7e ->
				    [Digit, 33, LowLetter, HiLetter, Other];
			       true ->
				    [Digit, Printable, LowLetter, HiLetter, 128]
			    end
		    end, [0, 0, 0, 0, 0], S),
	    length(S) * math:log(lists:sum(Set))/math:log(2)
=======
entropy(B) ->
    case binary_to_list(B) of
      "" -> 0.0;
      S ->
	  Set = lists:foldl(fun (C,
				 [Digit, Printable, LowLetter, HiLetter,
				  Other]) ->
				    if C >= $a, C =< $z ->
					   [Digit, Printable, 26, HiLetter,
					    Other];
				       C >= $0, C =< $9 ->
					   [9, Printable, LowLetter, HiLetter,
					    Other];
				       C >= $A, C =< $Z ->
					   [Digit, Printable, LowLetter, 26,
					    Other];
				       C >= 33, C =< 126 ->
					   [Digit, 33, LowLetter, HiLetter,
					    Other];
				       true ->
					   [Digit, Printable, LowLetter,
					    HiLetter, 128]
				    end
			    end,
			    [0, 0, 0, 0, 0], S),
	  length(S) * math:log(lists:sum(Set)) / math:log(2)
>>>>>>> upstream/master
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% @spec () -> [authmodule()]
%% @doc Return the lists of all the auth modules actually used in the
%% configuration.

auth_modules() ->
    lists:usort(lists:flatmap(fun (Server) ->
				      auth_modules(Server)
			      end,
			      ?MYHOSTS)).

-spec auth_modules(binary()) -> [atom()].

%% @spec (Server) -> [authmodule()]
%%     Server = string()
%% @doc Return the list of authenticated modules for a given host.

auth_modules(Server) ->
<<<<<<< HEAD
    Method = ejabberd_config:get_local_option({auth_method, Server}),
    Methods = if
		  Method == undefined -> [];
		  is_list(Method) -> Method;
		  is_atom(Method) -> [Method]
	      end,
    [module_name(M) || M <- Methods].

module_name(Method) when is_atom(Method) ->
    list_to_atom("ejabberd_auth_" ++ atom_to_list(Method)).
=======
    LServer = jlib:nameprep(Server),
    Methods = ejabberd_config:get_option(
                {auth_method, LServer},
                fun(V) when is_list(V) ->
                        true = lists:all(fun is_atom/1, V),
                        V;
                   (V) when is_atom(V) ->
                        [V]
                end, []),
    [jlib:binary_to_atom(<<"ejabberd_auth_",
                           (jlib:atom_to_binary(M))/binary>>)
     || M <- Methods].

export(Server) ->
    ejabberd_auth_internal:export(Server).

import(Server) ->
    ejabberd_auth_internal:import(Server).

import(Server, mnesia, Passwd) ->
    ejabberd_auth_internal:import(Server, mnesia, Passwd);
import(_, _, _) ->
    pass.
>>>>>>> upstream/master
