%%%----------------------------------------------------------------------
%%% File    : cyrsasl_digest.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : DIGEST-MD5 SASL mechanism
%%% Created : 11 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
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

-module(cyrsasl_digest).

-author('alexey@sevcom.net').

<<<<<<< HEAD
-export([start/1,
	 stop/0,
	 mech_new/1,
	 mech_step/2]).

-include("ejabberd.hrl").
-include("cyrsasl.hrl").

-behaviour(cyrsasl).

%% @type mechstate() = {state, Step, Nonce, Username, AuthzId, GetPassword, CheckPassword, AuthModule, Host}
%%     Step = 1 | 3 | 5
%%     Nonce = string()
%%     Username = string()
%%     AuthzId = string()
%%     GetPassword = function()
%%     AuthModule = atom()
%%     Host = string().

-record(state, {step, nonce, username, authzid, get_password, check_password, auth_module,
		host}).
=======
-export([start/1, stop/0, mech_new/4, mech_step/2, parse/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-behaviour(cyrsasl).

-type get_password_fun() :: fun((binary()) -> {false, any()} |
                                              {binary(), atom()}).

-type check_password_fun() :: fun((binary(), binary(), binary(),
                                   fun((binary()) -> binary())) ->
                                           {boolean(), any()} |
                                           false).

-record(state, {step = 1 :: 1 | 3 | 5,
                nonce = <<"">> :: binary(),
                username = <<"">> :: binary(),
                authzid = <<"">> :: binary(),
                get_password = fun(_) -> {false, <<>>} end :: get_password_fun(),
                check_password = fun(_, _, _, _) -> false end :: check_password_fun(),
                auth_module :: atom(),
                host = <<"">> :: binary(),
                hostfqdn = <<"">> :: binary()}).
>>>>>>> upstream/master

%% @spec (Opts) -> true
%%     Opts = term()

start(_Opts) ->
<<<<<<< HEAD
    cyrsasl:register_mechanism("DIGEST-MD5", ?MODULE, digest).

%% @spec () -> ok
=======
    Fqdn = get_local_fqdn(),
    ?INFO_MSG("FQDN used to check DIGEST-MD5 SASL authentication: ~s",
	      [Fqdn]),
    cyrsasl:register_mechanism(<<"DIGEST-MD5">>, ?MODULE,
			       digest).
>>>>>>> upstream/master

stop() -> ok.

<<<<<<< HEAD
mech_new(#sasl_params{host=Host, get_password=GetPassword,
		      check_password_digest=CheckPasswordDigest}) ->
    {ok, #state{step = 1,
		nonce = randoms:get_string(),
		host = Host,
		get_password = GetPassword,
		check_password = CheckPasswordDigest}}.
=======
mech_new(Host, GetPassword, _CheckPassword,
	 CheckPasswordDigest) ->
    {ok,
     #state{step = 1, nonce = randoms:get_string(),
	    host = Host, hostfqdn = get_local_fqdn(),
	    get_password = GetPassword,
	    check_password = CheckPasswordDigest}}.
>>>>>>> upstream/master

%% @spec (State, ClientIn) -> Ok | Continue | Error
%%     State = mechstate()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = {username, Username} | {authzid, AuthzId} | {auth_module, AuthModule}
%%         Username = string()
%%         AuthzId = string()
%%         AuthModule = atom()
%%     Continue = {continue, ServerOut, New_State}
%%         ServerOut = string()
%%         New_State = mechstate()
%%     Error = {error, Reason} | {error, Reason, Text, Username}
%%         Reason = term()

mech_step(#state{step = 1, nonce = Nonce} = State, _) ->
    {continue,
     <<"nonce=\"", Nonce/binary,
       "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess">>,
     State#state{step = 3}};
mech_step(#state{step = 3, nonce = Nonce} = State,
	  ClientIn) ->
    case parse(ClientIn) of
<<<<<<< HEAD
	bad ->
	    {error, 'malformed-request'};
	KeyVals ->
	    DigestURI = proplists:get_value("digest-uri", KeyVals, ""),
	    UserName = proplists:get_value("username", KeyVals, ""),
	    case is_digesturi_valid(DigestURI, State#state.host) of
		false ->
		    ?DEBUG("User login not authorized because digest-uri "
			   "seems invalid: ~p", [DigestURI]),
		    {error, 'not-authorized', "", UserName};
		true ->
		    AuthzId = proplists:get_value("authzid", KeyVals, ""),
		    case (State#state.get_password)(UserName) of
			{false, _} ->
			    {error, 'not-authorized', "", UserName};
			{Passwd, AuthModule} ->
				case (State#state.check_password)(UserName, "",
					proplists:get_value("response", KeyVals, ""),
					fun(PW) -> response(KeyVals, UserName, PW, Nonce, AuthzId,
						"AUTHENTICATE") end) of
				{true, _} ->
				    RspAuth = response(KeyVals,
						       UserName, Passwd,
						       Nonce, AuthzId, ""),
				    {continue,
				     "rspauth=" ++ RspAuth,
				     State#state{step = 5,
						 auth_module = AuthModule,
						 username = UserName,
						 authzid = AuthzId}};
				false ->
				    {error, 'not-authorized', "", UserName};
				{false, _} ->
				    {error, 'not-authorized', "", UserName}
			    end
		    end
	    end
=======
      bad -> {error, <<"bad-protocol">>};
      KeyVals ->
	  DigestURI = proplists:get_value(<<"digest-uri">>, KeyVals, <<>>),
	  %DigestURI = xml:get_attr_s(<<"digest-uri">>, KeyVals),
	  UserName = proplists:get_value(<<"username">>, KeyVals, <<>>),
	  %UserName = xml:get_attr_s(<<"username">>, KeyVals),
	  case is_digesturi_valid(DigestURI, State#state.host,
				  State#state.hostfqdn)
	      of
	    false ->
		?DEBUG("User login not authorized because digest-uri "
		       "seems invalid: ~p (checking for Host "
		       "~p, FQDN ~p)",
		       [DigestURI, State#state.host, State#state.hostfqdn]),
		{error, <<"not-authorized">>, UserName};
	    true ->
		AuthzId = proplists:get_value(<<"authzid">>, KeyVals, <<>>),
		%AuthzId = xml:get_attr_s(<<"authzid">>, KeyVals),
		case (State#state.get_password)(UserName) of
		  {false, _} -> {error, <<"not-authorized">>, UserName};
		  {Passwd, AuthModule} ->
		      case (State#state.check_password)(UserName, <<"">>,
		                    proplists:get_value(<<"response">>, KeyVals, <<>>),
							%xml:get_attr_s(<<"response">>, KeyVals),
							fun (PW) ->
								response(KeyVals,
									 UserName,
									 PW,
									 Nonce,
									 AuthzId,
									 <<"AUTHENTICATE">>)
							end)
			  of
			{true, _} ->
			    RspAuth = response(KeyVals, UserName, Passwd, Nonce,
					       AuthzId, <<"">>),
			    {continue, <<"rspauth=", RspAuth/binary>>,
			     State#state{step = 5, auth_module = AuthModule,
					 username = UserName,
					 authzid = AuthzId}};
			false -> {error, <<"not-authorized">>, UserName};
			{false, _} -> {error, <<"not-authorized">>, UserName}
		      end
		end
	  end
>>>>>>> upstream/master
    end;
mech_step(#state{step = 5, auth_module = AuthModule,
		 username = UserName, authzid = AuthzId},
	  <<"">>) ->
    {ok,
     [{username, UserName}, {authzid, AuthzId},
      {auth_module, AuthModule}]};
mech_step(A, B) ->
<<<<<<< HEAD
    ?DEBUG("SASL DIGEST: A ~p B ~p", [A,B]),
    {error, 'malformed-request'}.

%% @spec (S) -> [{Key, Value}] | bad
%%     S = string()
%%     Key = string()
%%     Value = string()
=======
    ?DEBUG("SASL DIGEST: A ~p B ~p", [A, B]),
    {error, <<"bad-protocol">>}.
>>>>>>> upstream/master

parse(S) -> parse1(binary_to_list(S), "", []).

%% @hidden

parse1([$= | Cs], S, Ts) ->
    parse2(Cs, lists:reverse(S), "", Ts);
<<<<<<< HEAD
parse1([$, | Cs], [], Ts) ->
    parse1(Cs, [], Ts);
parse1([$\s | Cs], [], Ts) ->
    parse1(Cs, [], Ts);
parse1([C | Cs], S, Ts) ->
    parse1(Cs, [C | S], Ts);
parse1([], [], T) ->
    lists:reverse(T);
parse1([], _S, _T) ->
    bad.

%% @hidden

parse2([$\" | Cs], Key, Val, Ts) ->
=======
parse1([$, | Cs], [], Ts) -> parse1(Cs, [], Ts);
parse1([$\s | Cs], [], Ts) -> parse1(Cs, [], Ts);
parse1([C | Cs], S, Ts) -> parse1(Cs, [C | S], Ts);
parse1([], [], T) -> lists:reverse(T);
parse1([], _S, _T) -> bad.

parse2([$" | Cs], Key, Val, Ts) ->
>>>>>>> upstream/master
    parse3(Cs, Key, Val, Ts);
parse2([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse2([], _, _, _) -> bad.

<<<<<<< HEAD
%% @hidden

parse3([$\" | Cs], Key, Val, Ts) ->
=======
parse3([$" | Cs], Key, Val, Ts) ->
>>>>>>> upstream/master
    parse4(Cs, Key, Val, Ts);
parse3([$\\, C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([], _, _, _) -> bad.

%% @hidden

parse4([$, | Cs], Key, Val, Ts) ->
    parse1(Cs, "", [{list_to_binary(Key), list_to_binary(lists:reverse(Val))} | Ts]);
parse4([$\s | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse4([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse4([], Key, Val, Ts) ->
<<<<<<< HEAD
    parse1([], "", [{Key, lists:reverse(Val)} | Ts]).


%% @spec (DigestURICase, JabberHost) -> bool()
%%     DigestURICase = string()
%%     JabberHost = string()
%%
=======
>>>>>>> upstream/master
%% @doc Check if the digest-uri is valid.
%% RFC-2831 allows to provide the IP address in Host,
%% however ejabberd doesn't allow that.
%% If the service (for example jabber.example.org)
%% is provided by several hosts (being one of them server3.example.org),
<<<<<<< HEAD
%% then digest-uri can be like xmpp/server3.example.org/jabber.example.org
%% In that case, ejabberd only checks the service name, not the host.

is_digesturi_valid(DigestURICase, JabberHost) ->
    DigestURI = exmpp_stringprep:to_lower(DigestURICase),
    case catch string:tokens(DigestURI, "/") of
	["xmpp", Host] when Host == JabberHost ->
	    true;
	["xmpp", _Host, ServName] when ServName == JabberHost ->
	    true;
=======
%% then acceptable digest-uris would be:
%% xmpp/server3.example.org/jabber.example.org, xmpp/server3.example.org and
%% xmpp/jabber.example.org
%% The last version is not actually allowed by the RFC, but implemented by popular clients
    parse1([], "", [{list_to_binary(Key), list_to_binary(lists:reverse(Val))} | Ts]).

is_digesturi_valid(DigestURICase, JabberDomain,
		   JabberFQDN) ->
    DigestURI = stringprep:tolower(DigestURICase),
    case catch str:tokens(DigestURI, <<"/">>) of
	[<<"xmpp">>, Host] ->
	    IsHostFqdn = is_host_fqdn(binary_to_list(Host), binary_to_list(JabberFQDN)),
	    (Host == JabberDomain) or IsHostFqdn;
	[<<"xmpp">>, Host, ServName] ->
	    IsHostFqdn = is_host_fqdn(binary_to_list(Host), binary_to_list(JabberFQDN)),
	    (ServName == JabberDomain) and IsHostFqdn;
>>>>>>> upstream/master
	_ ->
	    false
    end.

is_host_fqdn(Host, [Letter | _Tail] = Fqdn) when not is_list(Letter) ->
    Host == Fqdn;
is_host_fqdn(_Host, []) ->
    false;
is_host_fqdn(Host, [Fqdn | _FqdnTail]) when Host == Fqdn ->
    true;
is_host_fqdn(Host, [Fqdn | FqdnTail]) when Host /= Fqdn ->
    is_host_fqdn(Host, FqdnTail).

get_local_fqdn() ->
    case catch get_local_fqdn2() of
      Str when is_binary(Str) -> Str;
      _ ->
	  <<"unknown-fqdn, please configure fqdn "
	    "option in ejabberd.yml!">>
    end.

<<<<<<< HEAD

%% @hidden

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.
=======
get_local_fqdn2() ->
    case ejabberd_config:get_option(
           fqdn, fun iolist_to_binary/1) of
        ConfiguredFqdn when is_binary(ConfiguredFqdn) ->
            ConfiguredFqdn;
        undefined ->
            {ok, Hostname} = inet:gethostname(),
            {ok, {hostent, Fqdn, _, _, _, _}} =
            inet:gethostbyname(Hostname),
            list_to_binary(Fqdn)
    end.
>>>>>>> upstream/master

%% @hidden

hex(S) ->
<<<<<<< HEAD
    hex(S, []).

%% @hidden

hex([], Res) ->
    lists:reverse(Res);
hex([N | Ns], Res) ->
    hex(Ns, [digit_to_xchar(N rem 16),
	     digit_to_xchar(N div 16) | Res]).
=======
    p1_sha:to_hexlist(S).
>>>>>>> upstream/master

proplists_get_bin_value(Key, Pairs, Default) ->
    case proplists:get_value(Key, Pairs, Default) of
        L when is_list(L) ->
            list_to_binary(L);
        L2 ->
            L2
    end.

<<<<<<< HEAD
%% @spec (KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) -> string()
%%     KeyVals = [{Key, Value}]
%%         Key = string()
%%         Value = string()
%%     User = string()
%%     Passwd = string()
%%     Nonce = string()
%%     AuthzId = nil() | string()
%%     A2Prefix = string()

response(KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) ->
    Realm = proplists:get_value("realm", KeyVals, ""),
    CNonce = proplists:get_value("cnonce", KeyVals, ""),
    DigestURI = proplists:get_value("digest-uri", KeyVals, ""),
    NC = proplists:get_value("nc", KeyVals, ""),
    QOP = proplists:get_value("qop", KeyVals, ""),
    %% handle non-fully latin-1 strings as specified                             
    %% on RFC 2831 Section 2.1.2.1 (EJAB-476)                                    
    SUser = sanitize(User),
    SPasswd = sanitize(Passwd),
    SRealm = sanitize(Realm),
    A1 = case AuthzId of
	     "" ->
		 binary_to_list(
		   crypto:md5(SUser ++ ":" ++ SRealm ++ ":" ++ SPasswd)) ++
		     ":" ++ Nonce ++ ":" ++ CNonce;
	     _ ->
		 binary_to_list(
		   crypto:md5(SUser ++ ":" ++ SRealm ++ ":" ++ SPasswd)) ++
		     ":" ++ Nonce ++ ":" ++ CNonce ++ ":" ++ AuthzId
=======
response(KeyVals, User, Passwd, Nonce, AuthzId,
	 A2Prefix) ->
    Realm = proplists_get_bin_value(<<"realm">>, KeyVals, <<>>),
    CNonce = proplists_get_bin_value(<<"cnonce">>, KeyVals, <<>>),
    DigestURI = proplists_get_bin_value(<<"digest-uri">>, KeyVals, <<>>),
    NC = proplists_get_bin_value(<<"nc">>, KeyVals, <<>>),
    QOP = proplists_get_bin_value(<<"qop">>, KeyVals, <<>>),
    MD5Hash = erlang:md5(<<User/binary, ":", Realm/binary, ":",
                           Passwd/binary>>),
    A1 = case AuthzId of
	   <<"">> ->
	       <<MD5Hash/binary, ":", Nonce/binary, ":", CNonce/binary>>;
	   _ ->
	       <<MD5Hash/binary, ":", Nonce/binary, ":", CNonce/binary, ":",
		 AuthzId/binary>>
>>>>>>> upstream/master
	 end,
    A2 = case QOP of
	   <<"auth">> ->
	       <<A2Prefix/binary, ":", DigestURI/binary>>;
	   _ ->
	       <<A2Prefix/binary, ":", DigestURI/binary,
		 ":00000000000000000000000000000000">>
	 end,
<<<<<<< HEAD
    T = hex(binary_to_list(crypto:md5(A1))) ++ ":" ++ Nonce ++ ":" ++
	NC ++ ":" ++ CNonce ++ ":" ++ QOP ++ ":" ++
	hex(binary_to_list(crypto:md5(A2))),
    hex(binary_to_list(crypto:md5(T))).


sanitize(V) ->
    L = from_utf8(V),
    case lists:all(fun is_latin1/1, L) of
       true -> L;
       false -> V
    end.

%%%% copied from xmerl_ucs:from_utf8/1 and xmerl_ucs:is_latin1/1 , to not
%%%% require xmerl as a dependency only for this.

from_utf8(Bin) when is_binary(Bin) -> from_utf8(binary_to_list(Bin));
from_utf8(List) ->
    case expand_utf8(List) of
    {Result,0} -> Result;
    {_Res,_NumBadChar} ->
        exit({ucs,{bad_utf8_character_code}})
    end.



%% expand_utf8([Byte]) -> {[UnicodeChar],NumberOfBadBytes}
%%  Expand UTF8 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%%  Reference:
%%     RFC 3629: "UTF-8, a transformation format of ISO 10646".

expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
                     C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
    C when 16#80 =< C ->
        expand_utf8_1(Cs, [C|Acc], Bad);
    _ ->
        %% Bad range.
        expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
                        C2 band 16#C0 =:= 16#80,
                        C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
    (C3 band 16#3F) of
    C when 16#800 =< C ->
        expand_utf8_1(Cs, [C|Acc], Bad);
    _ ->
        %% Bad range.
        expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
                           C2 band 16#C0 =:= 16#80,
                           C3 band 16#C0 =:= 16#80,
                           C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
    (C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
    C when 16#10000 =< C ->
        expand_utf8_1(Cs, [C|Acc], Bad);
    _ ->
        %% Bad range.
        expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([_|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {lists:reverse(Acc),Bad}.


%%% Test for legitimate Latin-1 code
is_latin1(Ch) when is_integer(Ch), Ch >= 0, Ch =< 255 -> true;
is_latin1(_) -> false.

=======
    T = <<(hex((erlang:md5(A1))))/binary, ":", Nonce/binary,
	  ":", NC/binary, ":", CNonce/binary, ":", QOP/binary,
	  ":", (hex((erlang:md5(A2))))/binary>>,
    hex((erlang:md5(T))).
>>>>>>> upstream/master
