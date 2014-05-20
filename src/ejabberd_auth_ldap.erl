%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_auth_ldap).

-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(ejabberd_auth).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

%% External exports
<<<<<<< HEAD
-export([start/1,
	 stop/1,
	 start_link/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users_number/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 store_type/0,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").
-include("eldap/eldap.hrl").

-record(state, {host,
		eldap_id,
		bind_eldap_id,
		servers,
		backups,
		port,
		tls_options,
		dn,
		password,
		base,
		uids,
		ufilter,
		lfilter, %% Local filter (performed by ejabberd, not LDAP)
                deref_aliases,
		dn_filter,
		dn_filter_attrs
	       }).

%% Unused callbacks.
handle_cast(_Request, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.
%% -----
=======
-export([start/1, stop/1, start_link/1, set_password/3,
	 check_password/3, check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0,
	 plain_password_required/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("eldap.hrl").
>>>>>>> upstream/master

-record(state,
	{host = <<"">>          :: binary(),
         eldap_id = <<"">>      :: binary(),
         bind_eldap_id = <<"">> :: binary(),
         servers = []           :: [binary()],
         backups = []           :: [binary()],
         port = ?LDAP_PORT      :: inet:port_number(),
	 tls_options = []       :: list(),
         dn = <<"">>            :: binary(),
         password = <<"">>      :: binary(),
         base = <<"">>          :: binary(),
         uids = []              :: [{binary()} | {binary(), binary()}],
         ufilter = <<"">>       :: binary(),
         sfilter = <<"">>       :: binary(),
	 lfilter                :: {any(), any()},
         deref_aliases = never  :: never | searching | finding | always,
         dn_filter              :: binary(),
         dn_filter_attrs = []   :: [binary()]}).

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

-define(LDAP_SEARCH_TIMEOUT, 5).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec (Host) -> term()
%%     Host = string()

start(Host) ->
<<<<<<< HEAD
    ?DEBUG("Starting ~p for ~p.", [?MODULE, Host]),
    Option = case ejabberd_config:get_host_option(Host, ldap_servers) of
		 undefined -> check_local_config(Host);
		 {host, _Host} -> nothing;
		 _ ->
		     {start, Host}
	     end,
    case Option of
	nothing -> ok;
	{start, Host2} ->
	    Proc = gen_mod:get_module_proc(Host2, ?MODULE),
	    ChildSpec = {
	      Proc, {?MODULE, start_link, [Host2]},
	      transient, 1000, worker, [?MODULE]
	     },
	    supervisor:start_child(ejabberd_sup, ChildSpec)
    end.

check_local_config(Host) ->
    case ejabberd_config:get_local_option({ldap_servers, Host}) of
	undefined ->
	    ?ERROR_MSG("Can't start ~p for host ~p: missing ldap_servers "
		       "configuration", [?MODULE, Host]),
	    {error, bad_config};
	X when is_list(X)->
	    {start, Host}
    end.

%% @spec (Host) -> term()
%%     Host = string()
=======
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).
>>>>>>> upstream/master

stop(Host) ->
    case ejabberd_config:get_host_option(Host, ldap_servers) of
	undefined -> ok;
	{host, _Host} -> ok;
	_ ->
	    Proc = gen_mod:get_module_proc(Host, ?MODULE),
	    gen_server:call(Proc, stop),
	    supervisor:terminate_child(ejabberd_sup, Proc),
	    supervisor:delete_child(ejabberd_sup, Proc)
    end.

%% @spec (Host) -> term()
%%     Host = string()

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

<<<<<<< HEAD
%% @hidden

terminate(_Reason, _State) ->
    ok.
=======
terminate(_Reason, _State) -> ok.
>>>>>>> upstream/master

%% @spec (Host) -> {ok, State}
%%     Host = string()
%%     State = term()

init(Host) ->
    State = parse_options(Host),
    eldap_pool:start_link(State#state.eldap_id,
<<<<<<< HEAD
		     State#state.servers,
		     State#state.backups,
		     State#state.port,
		     State#state.dn,
		     State#state.password,
		     State#state.tls_options),
    eldap_pool:start_link(State#state.bind_eldap_id,
		     State#state.servers,
		     State#state.backups,
		     State#state.port,
		     State#state.dn,
		     State#state.password,
		     State#state.tls_options),
    {ok, State}.

%% @spec () -> true

plain_password_required() ->
    true.
=======
			  State#state.servers, State#state.backups,
			  State#state.port, State#state.dn,
			  State#state.password, State#state.tls_options),
    eldap_pool:start_link(State#state.bind_eldap_id,
			  State#state.servers, State#state.backups,
			  State#state.port, State#state.dn,
			  State#state.password, State#state.tls_options),
    {ok, State}.

plain_password_required() -> true.

store_type() -> external.
>>>>>>> upstream/master

store_type() ->
	external.

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password(User, Server, Password) ->
    if Password == <<"">> -> false;
       true ->
	   case catch check_password_ldap(User, Server, Password)
	       of
	     {'EXIT', _} -> false;
	     Result -> Result
	   end
    end.

<<<<<<< HEAD
%% @spec (User, Server, Password, Digest, DigestGen) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Digest = string()
%%     DigestGen = function()

check_password(User, Server, Password, _Digest, _DigestGen) ->
    check_password(User, Server, Password).

%% @spec (User, Server, Password) -> {error, Reason} | ok
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Reason = term()

set_password(User, Server, Password) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, Server, State) of
	false ->
	    {error, user_not_found};
	DN ->
	    eldap_pool:modify_passwd(State#state.eldap_id, DN, Password)
=======
check_password(User, Server, Password, _Digest,
	       _DigestGen) ->
    check_password(User, Server, Password).

set_password(User, Server, Password) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, State) of
      false -> {error, user_not_found};
      DN ->
	  eldap_pool:modify_passwd(State#state.eldap_id, DN,
				   Password)
>>>>>>> upstream/master
    end.

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(ldap),
    lists:flatmap(fun (Server) ->
			  get_vh_registered_users(Server)
		  end,
		  Servers).

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server) ->
    case catch get_vh_registered_users_ldap(Server) of
      {'EXIT', _} -> [];
      Result -> Result
    end.

get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

%% @spec (Server) -> Users_Number
%%     Server = string()
%%     Users_Number = integer()

get_vh_registered_users_number(Server) ->
    length(get_vh_registered_users(Server)).

<<<<<<< HEAD
%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

get_password(_User, _Server) ->
    false.

%% @spec (User, Server) -> nil()
%%     User = string()
%%     Server = string()

get_password_s(_User, _Server) ->
    "".
=======
get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(_User, _Server) -> false.

get_password_s(_User, _Server) -> <<"">>.
>>>>>>> upstream/master

%% @spec (User, Server) -> true | false | {error, Error}
%%     User = string()
%%     Server = string()

is_user_exists(User, Server) ->
    case catch is_user_exists_ldap(User, Server) of
      {'EXIT', Error} -> {error, Error};
      Result -> Result
    end.

<<<<<<< HEAD
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
=======
remove_user(_User, _Server) -> {error, not_allowed}.

remove_user(_User, _Server, _Password) -> not_allowed.
>>>>>>> upstream/master

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password_ldap(User, Server, Password) ->
<<<<<<< HEAD
    {ok, State} = get_state(Server),
    case find_user_dn(User, Server, State) of
	false ->
	    false;
	DN ->
	    case eldap_pool:bind(State#state.bind_eldap_id, DN, Password) of
		ok -> true;
		_ -> false
	    end
    end.

%% We need an ?MODULE server state to use for queries. This will
%% either be Server if this is a statically configured host or the
%% Server for a different host if this is a dynamically configured
%% vhost.
%% The {ldap_vhost, Server} -> Host. ejabberd config option specifies
%% which actual ?MODULE server to use for a particular Host. The value
%% of the option if it is defined or Server by default.
get_state(Server) ->
    Host = case ejabberd_config:get_local_option({ldap_servers, Server}) of
	    {host, H} -> H;
	    _ -> Server
	end,
    eldap_utils:get_state(Host, ?MODULE).

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()
=======
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, State) of
      false -> false;
      DN ->
	  case eldap_pool:bind(State#state.bind_eldap_id, DN,
			       Password)
	      of
	    ok -> true;
	    _ -> false
	  end
    end.
>>>>>>> upstream/master

get_vh_registered_users_ldap(Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    UIDs = eldap_utils:uids_domain_subst(Server, State#state.uids),
    Eldap_ID = State#state.eldap_id,
<<<<<<< HEAD
    SearchFilter = build_sfilter(State, UIDs),
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(SearchFilter) of
		{ok, EldapFilter} ->
		    case eldap_pool:search(Eldap_ID,
                                           [{base, State#state.base},
                                            {filter, EldapFilter},
                                            {timeout, ?LDAP_SEARCH_TIMEOUT},
                                            {deref_aliases, State#state.deref_aliases},
                                            {attributes, ResAttrs}]) of
			#eldap_search_result{entries = Entries} ->
			    lists:flatmap(
			      fun(#eldap_entry{attributes = Attrs,
					       object_name = DN}) ->
				      case is_valid_dn(DN, Server, Attrs, State) of
					  false -> [];
					  _ ->
					      case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
						  "" -> [];
						  {User, UIDFormat} ->
						      case eldap_utils:get_user_part(User, UIDFormat) of
							  {ok, U} ->
							      try
								  [{exmpp_stringprep:nodeprep(U), exmpp_stringprep:nameprep(Server)}]
							      catch
								  _ ->
								      []
							      end;
							  _ -> []
						      end
					      end
=======
    Server = State#state.host,
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(State#state.sfilter) of
      {ok, EldapFilter} ->
	  case eldap_pool:search(Eldap_ID,
				 [{base, State#state.base},
				  {filter, EldapFilter},
				  {timeout, ?LDAP_SEARCH_TIMEOUT},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, ResAttrs}])
	      of
	    #eldap_search_result{entries = Entries} ->
		lists:flatmap(fun (#eldap_entry{attributes = Attrs,
						object_name = DN}) ->
				      case is_valid_dn(DN, Attrs, State) of
					false -> [];
					_ ->
					    case
					      eldap_utils:find_ldap_attrs(UIDs,
									  Attrs)
						of
					      <<"">> -> [];
					      {User, UIDFormat} ->
						  case
						    eldap_utils:get_user_part(User,
									      UIDFormat)
						      of
						    {ok, U} ->
							case jlib:nodeprep(U) of
							  error -> [];
							  LU ->
							      [{LU,
								jlib:nameprep(Server)}]
							end;
						    _ -> []
						  end
					    end
>>>>>>> upstream/master
				      end
			      end,
			      Entries);
	    _ -> []
	  end;
      _ -> []
    end.

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

is_user_exists_ldap(User, Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
<<<<<<< HEAD
    case find_user_dn(User, Server, State) of
		false -> false;
		_DN -> true
	end.
=======
    case find_user_dn(User, State) of
      false -> false;
      _DN -> true
    end.
>>>>>>> upstream/master

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

<<<<<<< HEAD
find_user_dn(User, Server, State) ->
    ResAttrs = result_attrs(State),
    UserFilter = build_ufilter(State, Server),
    case eldap_filter:parse(UserFilter, [{"%u", User}]) of
	{ok, Filter} ->
	    case eldap_pool:search(State#state.eldap_id,
				   [{base, State#state.base},
				    {filter, Filter},
                                    {deref_aliases, State#state.deref_aliases},
				    {attributes, ResAttrs}]) of
		#eldap_search_result{entries = [#eldap_entry{attributes = Attrs,
							     object_name = DN} | _]} ->
			dn_filter(DN, Server, Attrs, State);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

%% apply the dn filter and the local filter:
dn_filter(DN, Server, Attrs, State) ->
    %% Check if user is denied access by attribute value (local check)
    case check_local_filter(Attrs, State) of
        false -> false;
        true -> is_valid_dn(DN, Server, Attrs, State)
    end.

%% Check that the DN is valid, based on the dn filter
is_valid_dn(DN, _, _, #state{dn_filter = undefined}) ->
    DN;

is_valid_dn(DN, Server, Attrs, State) ->
    DNAttrs = State#state.dn_filter_attrs,
    UIDs = eldap_utils:uids_domain_subst(Server, State#state.uids),
    Values = [{"%s", eldap_utils:get_ldap_attr(Attr, Attrs), 1} || Attr <- DNAttrs],
    SubstValues = case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
		      "" -> Values;
		      {S, UAF} ->
			  case eldap_utils:get_user_part(S, UAF) of
			      {ok, U} -> [{"%u", U} | Values];
			      _ -> Values
			  end
		  end ++ [{"%d", State#state.host}, {"%D", DN}],
    case eldap_filter:parse(State#state.dn_filter, SubstValues) of
	{ok, EldapFilter} ->
	    case eldap_pool:search(State#state.eldap_id,
                                   [{base, State#state.base},
                                    {filter, EldapFilter},
                                    {deref_aliases, State#state.deref_aliases},
                                    {attributes, ["dn"]}]) of
		#eldap_search_result{entries = [_|_]} ->
		    DN;
		_ ->
		    false
	    end;
	_ ->
	    false
=======
find_user_dn(User, State) ->
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(State#state.ufilter,
			    [{<<"%u">>, User}])
	of
      {ok, Filter} ->
	  case eldap_pool:search(State#state.eldap_id,
				 [{base, State#state.base}, {filter, Filter},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, ResAttrs}])
	      of
	    #eldap_search_result{entries =
				     [#eldap_entry{attributes = Attrs,
						   object_name = DN}
				      | _]} ->
		dn_filter(DN, Attrs, State);
	    _ -> false
	  end;
      _ -> false
    end.

%% apply the dn filter and the local filter:
dn_filter(DN, Attrs, State) ->
    case check_local_filter(Attrs, State) of
      false -> false;
      true -> is_valid_dn(DN, Attrs, State)
    end.

%% Check that the DN is valid, based on the dn filter
is_valid_dn(DN, _, #state{dn_filter = undefined}) -> DN;
is_valid_dn(DN, Attrs, State) ->
    DNAttrs = State#state.dn_filter_attrs,
    UIDs = State#state.uids,
    Values = [{<<"%s">>,
	       eldap_utils:get_ldap_attr(Attr, Attrs), 1}
	      || Attr <- DNAttrs],
    SubstValues = case eldap_utils:find_ldap_attrs(UIDs,
						   Attrs)
		      of
		    <<"">> -> Values;
		    {S, UAF} ->
			case eldap_utils:get_user_part(S, UAF) of
			  {ok, U} -> [{<<"%u">>, U} | Values];
			  _ -> Values
			end
		  end
		    ++ [{<<"%d">>, State#state.host}, {<<"%D">>, DN}],
    case eldap_filter:parse(State#state.dn_filter,
			    SubstValues)
	of
      {ok, EldapFilter} ->
	  case eldap_pool:search(State#state.eldap_id,
				 [{base, State#state.base},
				  {filter, EldapFilter},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, [<<"dn">>]}])
	      of
	    #eldap_search_result{entries = [_ | _]} -> DN;
	    _ -> false
	  end;
      _ -> false
>>>>>>> upstream/master
    end.

%% The local filter is used to check an attribute in ejabberd
%% and not in LDAP to limit the load on the LDAP directory.
%% A local rule can be either:
%%    {equal, {"accountStatus",["active"]}}
%%    {notequal, {"accountStatus",["disabled"]}}
%% {ldap_local_filter, {notequal, {"accountStatus",["disabled"]}}}
check_local_filter(_Attrs,
		   #state{lfilter = undefined}) ->
    true;
check_local_filter(Attrs,
		   #state{lfilter = LocalFilter}) ->
    {Operation, FilterMatch} = LocalFilter,
    local_filter(Operation, Attrs, FilterMatch).

local_filter(equal, Attrs, FilterMatch) ->
    {Attr, Value} = FilterMatch,
    case lists:keysearch(Attr, 1, Attrs) of
      false -> false;
      {value, {Attr, Value}} -> true;
      _ -> false
    end;
local_filter(notequal, Attrs, FilterMatch) ->
    not local_filter(equal, Attrs, FilterMatch).

<<<<<<< HEAD
result_attrs(#state{uids = UIDs, dn_filter_attrs = DNFilterAttrs}) ->
    lists:foldl(
      fun({UID}, Acc) ->
	      [UID | Acc];
	 ({UID, _}, Acc) ->
	      [UID | Acc]
      end, DNFilterAttrs, UIDs).

build_ufilter(State, VHost) ->
    UIDs = eldap_utils:uids_domain_subst(VHost, State#state.uids),
    SubFilter = lists:flatten(eldap_utils:generate_subfilter(UIDs)),
    case State#state.ufilter of
	"" -> SubFilter;
	F -> "(&" ++ SubFilter ++ F ++ ")"
    end.

build_sfilter(State, FormattedUIDs) ->
    SubFilter = lists:flatten(eldap_utils:generate_subfilter(FormattedUIDs)),
    UserFilter = case State#state.ufilter of
		     "" -> SubFilter;
		     F -> "(&" ++ SubFilter ++ F ++ ")"
		 end,
    eldap_filter:do_sub(UserFilter, [{"%u", "*"}]).
=======
result_attrs(#state{uids = UIDs,
		    dn_filter_attrs = DNFilterAttrs}) ->
    lists:foldl(fun ({UID}, Acc) -> [UID | Acc];
		    ({UID, _}, Acc) -> [UID | Acc]
		end,
		DNFilterAttrs, UIDs).
>>>>>>> upstream/master

%%%----------------------------------------------------------------------
%%% Auxiliary functions
%%%----------------------------------------------------------------------
parse_options(Host) ->
<<<<<<< HEAD
    Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, ?MODULE)),
    Bind_Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, bind_ejabberd_auth_ldap)),
    LDAPServers = ejabberd_config:get_local_option({ldap_servers, Host}),
    LDAPBackups = case ejabberd_config:get_local_option({ldap_backups, Host}) of
		   undefined -> [];
		   Backups -> Backups
		   end,
    LDAPEncrypt = ejabberd_config:get_local_option({ldap_encrypt, Host}),
    LDAPTLSVerify = ejabberd_config:get_local_option({ldap_tls_verify, Host}),
    LDAPTLSCAFile = ejabberd_config:get_local_option({ldap_tls_cacertfile, Host}),
    LDAPTLSDepth = ejabberd_config:get_local_option({ldap_tls_depth, Host}),
    LDAPPort = case ejabberd_config:get_local_option({ldap_port, Host}) of
		   undefined -> case LDAPEncrypt of
				    tls -> ?LDAPS_PORT;
				    starttls -> ?LDAP_PORT;
				    _ -> ?LDAP_PORT
				end;
		   P -> P
	       end,
    RootDN = case ejabberd_config:get_local_option({ldap_rootdn, Host}) of
		 undefined -> "";
		 RDN -> RDN
	     end,
    Password = case ejabberd_config:get_local_option({ldap_password, Host}) of
		   undefined -> "";
		   Pass -> Pass
	       end,
    UIDs = case ejabberd_config:get_local_option({ldap_uids, Host}) of
	       undefined -> [{"uid", "%u"}];
	       UI -> eldap_utils:uids_domain_subst(Host, UI)
	   end,
    SubFilter = lists:flatten(eldap_utils:generate_subfilter(UIDs)),
    UserFilter = case ejabberd_config:get_local_option({ldap_filter, Host}) of
		     undefined -> "";
		     "" -> "";
		     F ->
                         eldap_utils:check_filter(F),
                         "(&" ++ SubFilter ++ F ++ ")"
		 end,
    LDAPBase = ejabberd_config:get_local_option({ldap_base, Host}),
    {DNFilter, DNFilterAttrs} =
	case ejabberd_config:get_local_option({ldap_dn_filter, Host}) of
	    undefined ->
		{undefined, []};
	    {DNF, undefined} ->
		{DNF, []};
	    {DNF, DNFA} ->
		{DNF, DNFA}
	end,
    eldap_utils:check_filter(DNFilter),
    LocalFilter = ejabberd_config:get_local_option({ldap_local_filter, Host}),
    DerefAliases = case ejabberd_config:get_local_option(
                          {ldap_deref_aliases, Host}) of
                       undefined -> never;
                       Val -> Val
                   end,
    #state{host = Host,
	   eldap_id = Eldap_ID,
	   bind_eldap_id = Bind_Eldap_ID,
	   servers = LDAPServers,
	   backups = LDAPBackups,
	   port = LDAPPort,
	   tls_options = [{encrypt, LDAPEncrypt},
			  {tls_verify, LDAPTLSVerify},
                          {tls_cacertfile, LDAPTLSCAFile},
                          {tls_depth, LDAPTLSDepth}],
	   dn = RootDN,
	   password = Password,
	   base = LDAPBase,
	   uids = UIDs,
	   ufilter = UserFilter,
	   lfilter = LocalFilter,
           deref_aliases = DerefAliases,
	   dn_filter = DNFilter,
	   dn_filter_attrs = DNFilterAttrs
	  }.
=======
    Cfg = eldap_utils:get_config(Host, []),
    Eldap_ID = jlib:atom_to_binary(gen_mod:get_module_proc(Host, ?MODULE)),
    Bind_Eldap_ID = jlib:atom_to_binary(
                      gen_mod:get_module_proc(Host, bind_ejabberd_auth_ldap)),
    UIDsTemp = eldap_utils:get_opt(
                 {ldap_uids, Host}, [],
                 fun(Us) ->
                         lists:map(
                           fun({U, P}) ->
                                   {iolist_to_binary(U),
                                    iolist_to_binary(P)};
                              ({U}) ->
                                   {iolist_to_binary(U)};
                              (U) ->
                                   {iolist_to_binary(U)}
                           end, lists:flatten(Us))
                 end, [{<<"uid">>, <<"%u">>}]),
    UIDs = eldap_utils:uids_domain_subst(Host, UIDsTemp),
    SubFilter =	eldap_utils:generate_subfilter(UIDs),
    UserFilter = case eldap_utils:get_opt(
                        {ldap_filter, Host}, [],
                        fun check_filter/1, <<"">>) of
                     <<"">> ->
			 SubFilter;
                     F ->
                         <<"(&", SubFilter/binary, F/binary, ")">>
                 end,
    SearchFilter = eldap_filter:do_sub(UserFilter,
				       [{<<"%u">>, <<"*">>}]),
    {DNFilter, DNFilterAttrs} =
        eldap_utils:get_opt({ldap_dn_filter, Host}, [],
                            fun({DNF, DNFA}) ->
                                    NewDNFA = case DNFA of
                                                  undefined ->
                                                      [];
                                                  _ ->
                                                      [iolist_to_binary(A)
                                                       || A <- DNFA]
                                              end,
                                    NewDNF = check_filter(DNF),
                                    {NewDNF, NewDNFA}
                            end, {undefined, []}),
    LocalFilter = eldap_utils:get_opt(
                    {ldap_local_filter, Host}, [], fun(V) -> V end),
    #state{host = Host, eldap_id = Eldap_ID,
           bind_eldap_id = Bind_Eldap_ID,
           servers = Cfg#eldap_config.servers,
	   backups = Cfg#eldap_config.backups,
           port = Cfg#eldap_config.port,
	   tls_options = Cfg#eldap_config.tls_options,
	   dn = Cfg#eldap_config.dn,
           password = Cfg#eldap_config.password,
           base = Cfg#eldap_config.base,
           deref_aliases = Cfg#eldap_config.deref_aliases,
	   uids = UIDs, ufilter = UserFilter,
	   sfilter = SearchFilter, lfilter = LocalFilter,
	   dn_filter = DNFilter, dn_filter_attrs = DNFilterAttrs}.

check_filter(F) ->
    NewF = iolist_to_binary(F),
    {ok, _} = eldap_filter:parse(NewF),
    NewF.
>>>>>>> upstream/master
