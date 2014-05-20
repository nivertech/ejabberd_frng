%%%----------------------------------------------------------------------
%%% File    : cyrsasl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Cyrus SASL-like library
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(cyrsasl).

-author('alexey@process-one.net').

<<<<<<< HEAD
-export([start/0,
	 register_mechanism/3,
	 listmech/1,
	 server_new/8,
	 server_start/3,
	 server_step/2]).

-include("cyrsasl.hrl").
-include("ejabberd.hrl").

%% @type saslmechanism() = {sasl_mechanism, Mechanism, Module, Require_Plain}
%%     Mechanism = string()
%%     Module = atom()
%%     Require_Plain = bool().
%% Registry entry of a supported SASL mechanism.

-record(sasl_mechanism, {mechanism, module, password_type}).

%% @type saslstate() = {sasl_state, Service, Myname, Realm, GetPassword, CheckPassword, CheckPasswordDigest, Mech_Mod, Mech_State}
%%     Service = string()
%%     Myname = string()
%%     Realm = string()
%%     GetPassword = function()
%%     CheckPassword = function()
%%     CheckPasswordDigest = any()
%%     Mech_Mod = atom()
%%     Mech_State = term().
%% State of this process.

-record(sasl_state, {service, myname,
		     mech_mod, mech_state, params}).
=======
-export([start/0, register_mechanism/3, listmech/1,
	 server_new/7, server_start/3, server_step/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%
-export_type([
    mechanism/0,
    mechanisms/0,
    sasl_mechanism/0
]).

-record(sasl_mechanism,
        {mechanism = <<"">>    :: mechanism() | '$1',
         module                :: atom(),
         password_type = plain :: password_type() | '$2'}).

-type(mechanism() :: binary()).
-type(mechanisms() :: [mechanism(),...]).
-type(password_type() :: plain | digest | scram).
-type(props() :: [{username, binary()} |
                  {authzid, binary()} |
                  {auth_module, atom()}]).

-type(sasl_mechanism() :: #sasl_mechanism{}).
>>>>>>> upstream/master

-record(sasl_state,
{
    service,
    myname,
    realm,
    get_password,
    check_password,
    check_password_digest,
    mech_mod,
    mech_state
}).

<<<<<<< HEAD
%% @hidden

behaviour_info(callbacks) ->
    [{mech_new, 1}, {mech_step, 2}];
behaviour_info(_Other) ->
    undefined.
=======
-callback mech_new(binary(), fun(), fun(), fun()) -> any().
-callback mech_step(any(), binary()) -> {ok, props()} |
                                        {ok, props(), binary()} |
                                        {continue, binary(), any()} |
                                        {error, binary()} |
                                        {error, binary(), binary()}.
>>>>>>> upstream/master

%% @spec () -> ok

start() ->
    ets:new(sasl_mechanism,
	    [named_table, public,
	     {keypos, #sasl_mechanism.mechanism}]),
    cyrsasl_plain:start([]),
    cyrsasl_digest:start([]),
    cyrsasl_scram:start([]),
    cyrsasl_anonymous:start([]),
    maybe_try_start_gssapi(),
    ok.

<<<<<<< HEAD
maybe_try_start_gssapi() ->
    case os:getenv("KRB5_KTNAME") of
        false ->
	    ok;
        _String ->
	    try_start_gssapi()
    end.

try_start_gssapi() ->
    case code:load_file(esasl) of
	{module, _Module} ->
	    cyrsasl_gssapi:start([]);
	{error, What} ->
	    ?ERROR_MSG("Support for GSSAPI not started because esasl.beam was not found: ~p", [What])
    end.

%% @spec (Mechanism, Module, Require_Plain) -> true
%%     Mechanism = string()
%%     Module = atom()
%%     Require_Plain = bool()

register_mechanism(Mechanism, Module, PasswordType) ->
    ets:insert(sasl_mechanism,
	       #sasl_mechanism{mechanism = Mechanism,
			       module = Module,
			       password_type = PasswordType}).

% TODO use callbacks
%-include("ejabberd.hrl").
%-include("jlib.hrl").
%check_authzid(_State, Props) ->
%    AuthzId = xml:get_attr_s(authzid, Props),
%    case jlib:string_to_jid(AuthzId) of
%	error ->
%	    {error, "invalid-authzid"};
%	JID ->
%	    LUser = jlib:nodeprep(xml:get_attr_s(username, Props)),
%	    {U, S, R} = jlib:short_prepd_jid(JID),
%	    case R of
%		"" ->
%		    {error, "invalid-authzid"};
%		_ ->
%		    case {LUser, ?MYNAME} of
%			{U, S} ->
%			    ok;
%			_ ->
%			    {error, "invalid-authzid"}
%		    end
%	    end
%    end.

%% @spec (State, Props) -> ok | {error, 'not-authorized'}
%%     State = saslstate()
%%     Props = [{Key, Value}]
%%         Key = atom()
%%         Value = string()

check_credentials(_State, Props) ->
    case proplists:get_value(username, Props) of
	undefined ->
	    {error, 'not-authorized'};
	User ->
	    case exmpp_stringprep:is_node(User) of
		false -> {error, 'not-authorized'};
		true  -> ok
	    end
    end.

%% @spec (Host) -> [Mechanism]
%%     Host = string()
%%     Mechanism = string()
=======
%%
-spec(register_mechanism/3 ::
(
  Mechanim     :: mechanism(),
  Module       :: module(),
  PasswordType :: password_type())
    -> any()
).

register_mechanism(Mechanism, Module, PasswordType) ->
    ets:insert(sasl_mechanism,
	       #sasl_mechanism{mechanism = Mechanism, module = Module,
			       password_type = PasswordType}).

%%% TODO: use callbacks
%%-include("ejabberd.hrl").
%%-include("jlib.hrl").
%%check_authzid(_State, Props) ->
%%    AuthzId = xml:get_attr_s(authzid, Props),
%%    case jlib:string_to_jid(AuthzId) of
%%	error ->
%%	    {error, "invalid-authzid"};
%%	JID ->
%%	    LUser = jlib:nodeprep(xml:get_attr_s(username, Props)),
%%	    {U, S, R} = jlib:jid_tolower(JID),
%%	    case R of
%%		"" ->
%%		    {error, "invalid-authzid"};
%%		_ ->
%%		    case {LUser, ?MYNAME} of
%%			{U, S} ->
%%			    ok;
%%			_ ->
%%			    {error, "invalid-authzid"}
%%		    end
%%	    end
%%    end.

check_credentials(_State, Props) ->
    User = proplists:get_value(username, Props, <<>>),
    case jlib:nodeprep(User) of
      error -> {error, <<"not-authorized">>};
      <<"">> -> {error, <<"not-authorized">>};
      _LUser -> ok
    end.

-spec(listmech/1 ::
(
  Host ::binary())
    -> Mechanisms::mechanisms()
).
>>>>>>> upstream/master

listmech(Host) ->
    Mechs = ets:select(sasl_mechanism,
		       [{#sasl_mechanism{mechanism = '$1',
<<<<<<< HEAD
					 password_type = '$2',
					 _ = '_'},
			 case catch ejabberd_auth:store_type(Host) of
			 external ->
			      [{'==', '$2', plain}];
			 scram ->
			      [{'/=', '$2', digest}];
			 {'EXIT',{undef,[{Module,store_type,[]} | _]}} ->
			      ?WARNING_MSG("~p doesn't implement the function store_type/0", [Module]),
			      [];
			 _Else ->
			      []
=======
					 password_type = '$2', _ = '_'},
			 case catch ejabberd_auth:store_type(Host) of
			   external -> [{'==', '$2', plain}];
			   scram -> [{'/=', '$2', digest}];
			   {'EXIT', {undef, [{Module, store_type, []} | _]}} ->
			       ?WARNING_MSG("~p doesn't implement the function store_type/0",
					    [Module]),
			       [];
			   _Else -> []
>>>>>>> upstream/master
			 end,
			 ['$1']}]),
    filter_anonymous(Host, Mechs).

%% @spec (Service, ServerFQDN, UserRealm, SecFlags, GetPassword, CheckPassword, CheckPasswordDigest, Socket) -> saslstate()
%%     Service = string()
%%     ServerFQDN = string()
%%     UserRealm = string()
%%     SecFlags = [term()]
%%     GetPassword = function()
%%     CheckPassword = function()

server_new(Service, ServerFQDN, UserRealm, _SecFlags,
<<<<<<< HEAD
	   GetPassword, CheckPassword, CheckPasswordDigest, Socket) ->
    Params = #sasl_params{
      host = ServerFQDN,
      realm = UserRealm,
      get_password = GetPassword,
      check_password = CheckPassword,
      check_password_digest= CheckPasswordDigest,
      socket = Socket
     },

    #sasl_state{service = Service,
		myname = ServerFQDN,
		params = Params}.


%% @spec (State, Mech, ClientIn) -> Ok | Continue | Error
%%     State = saslstate()
%%     Mech = string()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = [{Key, Value}]
%%         Key = atom()
%%         Value = string()
%%     Continue = {continue, ServerOut, New_State}
%%         ServerOut = string()
%%         New_State = saslstate()
%%     Error = {error, Reason} | {error, Reason, Username}
%%         Reason = term()
%%         Username = string()

server_start(State, Mech, ClientIn) ->
    case lists:member(Mech, listmech(State#sasl_state.myname)) of
	true ->
	    case ets:lookup(sasl_mechanism, Mech) of
		[#sasl_mechanism{module = Module}] ->
		    {ok, MechState} =
			Module:mech_new(State#sasl_state.params),
		    server_step(State#sasl_state{mech_mod = Module,
						 mech_state = MechState},
				ClientIn);
		_ ->
		    {error, 'invalid-mechanism'}
	    end;
	false ->
	    {error, 'invalid-mechanism'}
=======
	   GetPassword, CheckPassword, CheckPasswordDigest) ->
    #sasl_state{service = Service, myname = ServerFQDN,
		realm = UserRealm, get_password = GetPassword,
		check_password = CheckPassword,
		check_password_digest = CheckPasswordDigest}.

server_start(State, Mech, ClientIn) ->
    case lists:member(Mech,
		      listmech(State#sasl_state.myname))
	of
      true ->
	  case ets:lookup(sasl_mechanism, Mech) of
	    [#sasl_mechanism{module = Module}] ->
		{ok, MechState} =
		    Module:mech_new(State#sasl_state.myname,
				    State#sasl_state.get_password,
				    State#sasl_state.check_password,
				    State#sasl_state.check_password_digest),
		server_step(State#sasl_state{mech_mod = Module,
					     mech_state = MechState},
			    ClientIn);
	    _ -> {error, <<"no-mechanism">>}
	  end;
      false -> {error, <<"no-mechanism">>}
>>>>>>> upstream/master
    end.

%% @spec (State, ClientIn) -> Ok | Continue | Error
%%     State = saslstate()
%%     ClientIn = string()
%%     Ok = {ok, Props} | {ok, Props, ServerOut}
%%         Props = [Prop]
%%         Prop = [{Key, Value}]
%%         Key = atom()
%%         Value = string()
%%     Continue = {continue, ServerOut, New_State}
%%         ServerOut = string()
%%         New_State = saslstate()
%%     Error = {error, Reason} | {error, Reason, Text, Username}
%%         Reason = term()
%%         Text = string()
%%         Username = string()

server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
<<<<<<< HEAD
	{ok, Props} ->
	    case check_credentials(State, Props) of
		ok ->
		    {ok, Props};
		{error, Error} ->
		    {error, Error}
	    end;
	{ok, Props, ServerOut} ->
	    case check_credentials(State, Props) of
		ok ->
		    {ok, Props, ServerOut};
		{error, Error} ->
		    {error, Error}
	    end;
	{continue, ServerOut, NewMechState} ->
	    {continue, ServerOut,
	     State#sasl_state{mech_state = NewMechState}};
	{error, Error, Text, Username} ->
	    {error, Error, Text, Username};
	{error, Error} ->
	    {error, Error}
    end.

%% @spec (Host, Mechs) -> [Filtered_Mechs]
%%     Host = string()
%%     Mechs = [Mech]
%%         Mech = string()
%%     Filtered_Mechs = [Mech]
%%
%% @doc Remove the anonymous mechanism from the list if not enabled for
%% the given host.
=======
        {ok, Props} ->
            case check_credentials(State, Props) of
                ok             -> {ok, Props};
                {error, Error} -> {error, Error}
            end;
        {ok, Props, ServerOut} ->
            case check_credentials(State, Props) of
                ok             -> {ok, Props, ServerOut};
                {error, Error} -> {error, Error}
            end;
        {continue, ServerOut, NewMechState} ->
            {continue, ServerOut, State#sasl_state{mech_state = NewMechState}};
        {error, Error, Username} ->
            {error, Error, Username};
        {error, Error} ->
            {error, Error}
    end.

%% Remove the anonymous mechanism from the list if not enabled for the given
%% host
%%
-spec(filter_anonymous/2 ::
(
  Host  :: binary(),
  Mechs :: mechanisms())
    -> mechanisms()
).
>>>>>>> upstream/master

filter_anonymous(Host, Mechs) ->
    case ejabberd_auth_anonymous:is_sasl_anonymous_enabled(Host) of
      true  -> Mechs;
      false -> Mechs -- [<<"ANONYMOUS">>]
    end.
