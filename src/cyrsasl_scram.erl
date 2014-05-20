%%%----------------------------------------------------------------------
%%% File    : cyrsasl_scram.erl
%%% Author  : Stephen Röttger <stephen.roettger@googlemail.com>
%%% Purpose : SASL SCRAM authentication
%%% Created : 7 Aug 2011 by Stephen Röttger <stephen.roettger@googlemail.com>
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
<<<<<<< HEAD
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
=======
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
>>>>>>> upstream/master
%%%
%%%----------------------------------------------------------------------

-module(cyrsasl_scram).
<<<<<<< HEAD
-author('stephen.roettger@googlemail.com').

-export([start/1,
	 stop/0,
	 mech_new/1,
	 mech_step/2]).

-include("ejabberd.hrl").
-include("cyrsasl.hrl").

-behaviour(cyrsasl).

-record(state, {step, stored_key, server_key, username, get_password, check_password,
		auth_message, client_nonce, server_nonce}).

-define(SALT_LENGTH, 16).
-define(NONCE_LENGTH, 16).

start(_Opts) ->
    cyrsasl:register_mechanism("SCRAM-SHA-1", ?MODULE, scram).

stop() ->
    ok.

mech_new(#sasl_params{get_password=GetPassword}) ->
    {ok, #state{step = 2, get_password = GetPassword}}.

mech_step(#state{step = 2} = State, ClientIn) ->
	case string:tokens(ClientIn, ",") of
	[CBind, UserNameAttribute, ClientNonceAttribute] when (CBind == "y") or (CBind == "n") ->
		case parse_attribute(UserNameAttribute) of
		{error, Reason} ->
			{error, Reason};
		{_, EscapedUserName} ->
			case unescape_username(EscapedUserName) of
			error ->
				{error, 'malformed-request', "Error in username encoding", EscapedUserName};
			UserName ->
				case parse_attribute(ClientNonceAttribute) of
				{$r, ClientNonce} ->
					case (State#state.get_password)(UserName) of
					{false, _} ->
						{error, 'not-authorized', "", UserName};
					{Ret, _AuthModule} ->
						{StoredKey, ServerKey, Salt, IterationCount} = if
						is_tuple(Ret) ->
							Ret;
						true ->
							TempSalt = crypto:rand_bytes(?SALT_LENGTH),
							SaltedPassword = scram:salted_password(Ret, TempSalt, ?SCRAM_DEFAULT_ITERATION_COUNT),
							{scram:stored_key(scram:client_key(SaltedPassword)),
							 scram:server_key(SaltedPassword), TempSalt, ?SCRAM_DEFAULT_ITERATION_COUNT}
						end,
						ClientFirstMessageBare = string:substr(ClientIn, string:str(ClientIn, "n=")),
						ServerNonce = base64:encode_to_string(crypto:rand_bytes(?NONCE_LENGTH)),
						ServerFirstMessage = "r=" ++ ClientNonce ++ ServerNonce ++ "," ++
											"s=" ++ base64:encode_to_string(Salt) ++ "," ++
											"i=" ++ integer_to_list(IterationCount),
						{continue,
						 ServerFirstMessage,
						 State#state{step = 4, stored_key = StoredKey, server_key = ServerKey,
									 auth_message = ClientFirstMessageBare ++ "," ++ ServerFirstMessage,
									 client_nonce = ClientNonce, server_nonce = ServerNonce, username = UserName}}
					end;
				_Else ->
					{error, 'malformed-request'}
				end
			end
		end;
	_Else ->
	    {error, 'malformed-request'}
	end;
mech_step(#state{step = 4} = State, ClientIn) ->
	case string:tokens(ClientIn, ",") of
	[GS2ChannelBindingAttribute, NonceAttribute, ClientProofAttribute] ->
		case parse_attribute(GS2ChannelBindingAttribute) of
		{$c, CVal} when (CVal == "biws") or (CVal == "eSws") ->
		    %% biws is base64 for n,, => channelbinding not supported
		    %% eSws is base64 for y,, => channelbinding supported by client only
 			Nonce = State#state.client_nonce ++ State#state.server_nonce,
			case parse_attribute(NonceAttribute) of
			{$r, CompareNonce} when CompareNonce == Nonce ->
				case parse_attribute(ClientProofAttribute) of
				{$p, ClientProofB64} ->
					ClientProof = base64:decode(ClientProofB64),
					AuthMessage = State#state.auth_message ++ "," ++ string:substr(ClientIn, 1, string:str(ClientIn, ",p=")-1),
					ClientSignature = scram:client_signature(State#state.stored_key, AuthMessage),
					ClientKey = scram:client_key(ClientProof, ClientSignature),
					CompareStoredKey = scram:stored_key(ClientKey),
					if CompareStoredKey == State#state.stored_key ->
						ServerSignature = scram:server_signature(State#state.server_key, AuthMessage),
						{ok, [{username, State#state.username}], "v=" ++ base64:encode_to_string(ServerSignature)};
					true ->
						{error, 'not-authorized', "", State#state.username}
					end;
				_Else ->
					{error, 'malformed-request', "Bad protocol", State#state.username}
				end;
			{$r, _} ->
				{error, 'malformed-request', "Bad nonce", State#state.username};
			_Else ->
				{error, 'malformed-request', "Bad protocol", State#state.username}
			end;
		_Else ->
	   		{error, 'malformed-request', "Bad protocol", State#state.username}
		end;
	_Else ->
		{error, 'malformed-request', "Bad protocol", State#state.username}
	end.

parse_attribute(Attribute) ->
	AttributeLen = string:len(Attribute),
	if
	AttributeLen >= 3 ->
		SecondChar = lists:nth(2, Attribute),
		case is_alpha(lists:nth(1, Attribute)) of
			true ->
				if
				SecondChar == $= ->
					String = string:substr(Attribute, 3),
					{lists:nth(1, Attribute), String};
				true ->
					{error, 'malformed-request', "Second char not equal sign", ""}
				end;
			_Else ->
				{error, 'malformed-request', "First char not a letter", ""}
		end;
	true -> 
		{error, 'malformed-request', "Attribute too short", ""}
	end.

unescape_username("") ->
	"";
unescape_username(EscapedUsername) ->
	Pos = string:str(EscapedUsername, "="),
	if
	Pos == 0 ->
		EscapedUsername;
	true ->
		Start = string:substr(EscapedUsername, 1, Pos-1),
		End = string:substr(EscapedUsername, Pos),
		EndLen = string:len(End),
		if
		EndLen < 3 ->
			error;
		true ->
			case string:substr(End, 1, 3) of
			"=2C" ->
				Start ++ "," ++ unescape_username(string:substr(End, 4));
			"=3D" ->
				Start ++ "=" ++ unescape_username(string:substr(End, 4));
			_Else ->
				error
			end
		end
	end.

is_alpha(Char) when Char >= $a, Char =< $z ->
    true;
is_alpha(Char) when Char >= $A, Char =< $Z -> 
    true;
is_alpha(_) ->
    false.

=======

-author('stephen.roettger@googlemail.com').

-export([start/1, stop/0, mech_new/4, mech_step/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-behaviour(cyrsasl).

-record(state,
	{step = 2              :: 2 | 4,
         stored_key = <<"">>   :: binary(),
         server_key = <<"">>   :: binary(),
         username = <<"">>     :: binary(),
         get_password          :: fun(),
	 check_password        :: fun(),
         auth_message = <<"">> :: binary(),
         client_nonce = <<"">> :: binary(),
	 server_nonce = <<"">> :: binary()}).

-define(SALT_LENGTH, 16).

-define(NONCE_LENGTH, 16).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"SCRAM-SHA-1">>, ?MODULE,
			       scram).

stop() -> ok.

mech_new(_Host, GetPassword, _CheckPassword,
	 _CheckPasswordDigest) ->
    {ok, #state{step = 2, get_password = GetPassword}}.

mech_step(#state{step = 2} = State, ClientIn) ->
    case re:split(ClientIn, <<",">>, [{return, binary}]) of
      [_CBind, _AuthorizationIdentity, _UserNameAttribute, _ClientNonceAttribute, ExtensionAttribute | _]
	  when ExtensionAttribute /= [] ->
	  {error, <<"protocol-error-extension-not-supported">>};
      [CBind, _AuthorizationIdentity, UserNameAttribute, ClientNonceAttribute | _]
	  when (CBind == <<"y">>) or (CBind == <<"n">>) ->
	  case parse_attribute(UserNameAttribute) of
	    {error, Reason} -> {error, Reason};
	    {_, EscapedUserName} ->
		case unescape_username(EscapedUserName) of
		  error -> {error, <<"protocol-error-bad-username">>};
		  UserName ->
		      case parse_attribute(ClientNonceAttribute) of
			{$r, ClientNonce} ->
			    case (State#state.get_password)(UserName) of
			      {false, _} -> {error, <<"not-authorized">>, UserName};
			      {Ret, _AuthModule} ->
				  {StoredKey, ServerKey, Salt, IterationCount} =
				      if is_tuple(Ret) -> Ret;
					 true ->
					     TempSalt =
						 crypto:rand_bytes(?SALT_LENGTH),
					     SaltedPassword =
						 scram:salted_password(Ret,
								       TempSalt,
								       ?SCRAM_DEFAULT_ITERATION_COUNT),
					     {scram:stored_key(scram:client_key(SaltedPassword)),
					      scram:server_key(SaltedPassword),
					      TempSalt,
					      ?SCRAM_DEFAULT_ITERATION_COUNT}
				      end,
				  ClientFirstMessageBare =
				      str:substr(ClientIn,
                                                 str:str(ClientIn, <<"n=">>)),
				  ServerNonce =
				      jlib:encode_base64(crypto:rand_bytes(?NONCE_LENGTH)),
				  ServerFirstMessage =
                                        iolist_to_binary(
                                          ["r=",
                                           ClientNonce,
                                           ServerNonce,
                                           ",", "s=",
                                           jlib:encode_base64(Salt),
                                           ",", "i=",
                                           integer_to_list(IterationCount)]),
				  {continue, ServerFirstMessage,
				   State#state{step = 4, stored_key = StoredKey,
					       server_key = ServerKey,
					       auth_message =
						   <<ClientFirstMessageBare/binary,
						     ",", ServerFirstMessage/binary>>,
					       client_nonce = ClientNonce,
					       server_nonce = ServerNonce,
					       username = UserName}}
			    end;
			_Else -> {error, <<"not-supported">>}
		      end
		end
	  end;
      _Else -> {error, <<"bad-protocol">>}
    end;
mech_step(#state{step = 4} = State, ClientIn) ->
    case str:tokens(ClientIn, <<",">>) of
      [GS2ChannelBindingAttribute, NonceAttribute,
       ClientProofAttribute] ->
	  case parse_attribute(GS2ChannelBindingAttribute) of
	    {$c, CVal} ->
		ChannelBindingSupport = binary:at(jlib:decode_base64(CVal), 0),
		if (ChannelBindingSupport == $n)
		  or (ChannelBindingSupport == $y) ->
		    Nonce = <<(State#state.client_nonce)/binary,
				(State#state.server_nonce)/binary>>,
		    case parse_attribute(NonceAttribute) of
			{$r, CompareNonce} when CompareNonce == Nonce ->
			    case parse_attribute(ClientProofAttribute) of
			    {$p, ClientProofB64} ->
				  ClientProof = jlib:decode_base64(ClientProofB64),
				  AuthMessage = iolist_to_binary(
						    [State#state.auth_message,
						     ",",
						     str:substr(ClientIn, 1,
								    str:str(ClientIn, <<",p=">>)
								    - 1)]),
				  ClientSignature =
				    scram:client_signature(State#state.stored_key,
							     AuthMessage),
				  ClientKey = scram:client_key(ClientProof,
							     ClientSignature),
				  CompareStoredKey = scram:stored_key(ClientKey),
				  if CompareStoredKey == State#state.stored_key ->
					 ServerSignature =
					     scram:server_signature(State#state.server_key,
								    AuthMessage),
					 {ok, [{username, State#state.username}],
					  <<"v=",
					    (jlib:encode_base64(ServerSignature))/binary>>};
				     true -> {error, <<"bad-auth">>}
				  end;
			    _Else -> {error, <<"bad-protocol">>}
			    end;
			{$r, _} -> {error, <<"bad-nonce">>};
			_Else -> {error, <<"bad-protocol">>}
		    end;
		  true -> {error, <<"bad-channel-binding">>}
		end;
	    _Else -> {error, <<"bad-protocol">>}
	  end;
      _Else -> {error, <<"bad-protocol">>}
    end.

parse_attribute(Attribute) ->
    AttributeLen = byte_size(Attribute),
    if AttributeLen >= 3 ->
           AttributeS = binary_to_list(Attribute),
	   SecondChar = lists:nth(2, AttributeS),
	   case is_alpha(lists:nth(1, AttributeS)) of
	     true ->
		 if SecondChar == $= ->
			String = str:substr(Attribute, 3),
			{lists:nth(1, AttributeS), String};
		    true -> {error, <<"bad-format second char not equal sign">>}
		 end;
	     _Else -> {error, <<"bad-format first char not a letter">>}
	   end;
       true -> {error, <<"bad-format attribute too short">>}
    end.

unescape_username(<<"">>) -> <<"">>;
unescape_username(EscapedUsername) ->
    Pos = str:str(EscapedUsername, <<"=">>),
    if Pos == 0 -> EscapedUsername;
       true ->
	   Start = str:substr(EscapedUsername, 1, Pos - 1),
	   End = str:substr(EscapedUsername, Pos),
	   EndLen = byte_size(End),
	   if EndLen < 3 -> error;
	      true ->
		  case str:substr(End, 1, 3) of
		    <<"=2C">> ->
			<<Start/binary, ",",
			  (unescape_username(str:substr(End, 4)))/binary>>;
		    <<"=3D">> ->
			<<Start/binary, "=",
			  (unescape_username(str:substr(End, 4)))/binary>>;
		    _Else -> error
		  end
	   end
    end.

is_alpha(Char) when Char >= $a, Char =< $z -> true;
is_alpha(Char) when Char >= $A, Char =< $Z -> true;
is_alpha(_) -> false.
>>>>>>> upstream/master
