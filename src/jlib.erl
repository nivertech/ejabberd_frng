%%%----------------------------------------------------------------------
%%% File    : jlib.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : General XMPP library.
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

%% Some replacements to make in ejabberd source code to work with exmpp:
%% ```
%% - JID#jid.user
%% + exmpp_jid:prep_node(JID),
%% '''
%% ```
%% - JID#jid.server
%% + exmpp_jid:prep_domain(JID)
%% '''
%% ```
%% - ?SERR_INVALID_NAMESPACE
%% + exmpp_stream:error('invalid-namespace')
%% '''
%% ```
%% - ?POLICY_VIOLATION_ERR(Lang, "Use of STARTTLS required")
%% + exmpp_stream:error('policy-violation', {Lang, "Use of STARTTLS required"})
%% '''
%% ```
%% - IQ#iq{type = result, sub_el = Result}
%% + exmpp_iq:result(IQ, Result)
%% '''

-module(jlib).

-author('alexey@process-one.net').

<<<<<<< HEAD
-export([parse_xdata_submit/1,
	 timestamp_to_iso/1, % TODO: Remove once XEP-0091 is Obsolete
	 timestamp_to_iso/2,
	 timestamp_to_xml/4,
	 timestamp_to_xml/1, % TODO: Remove once XEP-0091 is Obsolete
	 now_to_utc_string/1,
	 now_to_local_string/1,
	 datetime_string_to_timestamp/1,
	 decode_base64/1,
	 encode_base64/1,
	 ip_to_list/1,
	 rsm_encode/1,
	 rsm_encode/2,
	 rsm_decode/1,
	 from_old_jid/1,
	 short_jid/1,
	 short_bare_jid/1,
	 short_prepd_jid/1,
	 short_prepd_bare_jid/1,
	 make_result_iq_reply/1, % TODO: still uses xmlelement
	 make_error_reply/3, % TODO: still uses xmlelement
	 make_error_reply/2, % TODO: still uses xmlelement
	 make_error_element/2, % TODO: still uses xmlelement
	 make_correct_from_to_attrs/3, % TODO: still uses xmlelement
	 replace_from_to_attrs/3, % TODO: still uses xmlelement
	 replace_from_to/3, % TODO: still uses xmlelement
	 replace_from_attrs/2, % TODO: still uses xmlelement
	 replace_from/2, % TODO: still uses xmlelement
	 remove_attr/2, % TODO: still uses xmlelement
	 make_jid/3,
	 make_jid/1,
	 string_to_jid/1,
	 jid_to_string/1,
	 is_nodename/1,
	 tolower/1,
	 nodeprep/1,
	 nameprep/1,
	 resourceprep/1,
	 jid_tolower/1,
	 jid_remove_resource/1,
	 jid_replace_resource/2,
	 get_iq_namespace/1, % TODO: still uses xmlelement
	 iq_query_info/1,
	 iq_query_or_response_info/1,
	 is_iq_request_type/1,
	 iq_to_xml/1
 ]).

-include_lib("exmpp/include/exmpp.hrl").

-include("jlib.hrl").

%% @type shortjid() = {U, S, R}
%%     U = binary()
%%     S = binary()
%%     R = binary().

parse_xdata_submit(#xmlel{attrs = Attrs, children = Els}) ->
    case exmpp_xml:get_attribute_from_list_as_list(Attrs, <<"type">>, "") of
	"submit" ->
	    lists:reverse(parse_xdata_fields(Els, []));
	"form" -> %% This is a workaround to accept Psi's wrong forms
	    lists:reverse(parse_xdata_fields(Els, []));
	_ ->
	    invalid
    end.

parse_xdata_fields([], Res) ->
    Res;
parse_xdata_fields([#xmlel{name = 'field', attrs = Attrs, children = SubEls} |
  Els], Res) ->
    case exmpp_xml:get_attribute_from_list_as_list(Attrs, <<"var">>, "") of
	"" ->
	    parse_xdata_fields(Els, Res);
	Var ->
	    Field = {Var, lists:reverse(parse_xdata_values(SubEls, []))},
	    parse_xdata_fields(Els, [Field | Res])
=======
-compile({no_auto_import, [atom_to_binary/2,
                           binary_to_integer/1,
                           integer_to_binary/1]}).

-export([make_result_iq_reply/1, make_error_reply/3,
	 make_error_reply/2, make_error_element/2,
	 make_correct_from_to_attrs/3, replace_from_to_attrs/3,
	 replace_from_to/3, replace_from_attrs/2, replace_from/2,
	 remove_attr/2, make_jid/3, make_jid/1, string_to_jid/1,
	 jid_to_string/1, is_nodename/1, tolower/1, nodeprep/1,
	 nameprep/1, resourceprep/1, jid_tolower/1,
	 jid_remove_resource/1, jid_replace_resource/2,
	 get_iq_namespace/1, iq_query_info/1,
	 iq_query_or_response_info/1, is_iq_request_type/1,
	 iq_to_xml/1, parse_xdata_submit/1, timestamp_to_iso/1,
	 timestamp_to_iso/2, timestamp_to_xml/4,
	 timestamp_to_xml/1, now_to_utc_string/1,
	 now_to_local_string/1, datetime_string_to_timestamp/1,
	 term_to_base64/1, base64_to_term/1,
	 decode_base64/1, encode_base64/1, ip_to_list/1,
	 rsm_encode/1, rsm_encode/2, rsm_decode/1,
	 binary_to_integer/1, binary_to_integer/2,
	 integer_to_binary/1, integer_to_binary/2,
	 atom_to_binary/1, binary_to_atom/1, tuple_to_binary/1,
	 l2i/1, i2l/1, i2l/2, queue_drop_while/2]).

%% TODO: Remove once XEP-0091 is Obsolete
%% TODO: Remove once XEP-0091 is Obsolete

-include("jlib.hrl").

-export_type([jid/0]).

%send_iq(From, To, ID, SubTags) ->
%    ok.

-spec make_result_iq_reply(xmlel()) -> xmlel().

make_result_iq_reply(#xmlel{name = Name, attrs = Attrs,
			    children = SubTags}) ->
    NewAttrs = make_result_iq_reply_attrs(Attrs),
    #xmlel{name = Name, attrs = NewAttrs,
	   children = SubTags}.

-spec make_result_iq_reply_attrs([attr()]) -> [attr()].

make_result_iq_reply_attrs(Attrs) ->
    To = xml:get_attr(<<"to">>, Attrs),
    From = xml:get_attr(<<"from">>, Attrs),
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = case To of
	       {value, ToVal} -> [{<<"from">>, ToVal} | Attrs2];
	       _ -> Attrs2
	     end,
    Attrs4 = case From of
	       {value, FromVal} -> [{<<"to">>, FromVal} | Attrs3];
	       _ -> Attrs3
	     end,
    Attrs5 = lists:keydelete(<<"type">>, 1, Attrs4),
    Attrs6 = [{<<"type">>, <<"result">>} | Attrs5],
    Attrs6.

-spec make_error_reply(xmlel(), binary(), binary()) -> xmlel().

make_error_reply(#xmlel{name = Name, attrs = Attrs,
			children = SubTags},
		 Code, Desc) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    #xmlel{name = Name, attrs = NewAttrs,
	   children =
	       SubTags ++
		 [#xmlel{name = <<"error">>,
			 attrs = [{<<"code">>, Code}],
			 children = [{xmlcdata, Desc}]}]}.

-spec make_error_reply(xmlel(), xmlel()) -> xmlel().

make_error_reply(#xmlel{name = Name, attrs = Attrs,
			children = SubTags},
		 Error) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    #xmlel{name = Name, attrs = NewAttrs,
	   children = SubTags ++ [Error]}.

-spec make_error_reply_attrs([attr()]) -> [attr()].

make_error_reply_attrs(Attrs) ->
    To = xml:get_attr(<<"to">>, Attrs),
    From = xml:get_attr(<<"from">>, Attrs),
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = case To of
	       {value, ToVal} -> [{<<"from">>, ToVal} | Attrs2];
	       _ -> Attrs2
	     end,
    Attrs4 = case From of
	       {value, FromVal} -> [{<<"to">>, FromVal} | Attrs3];
	       _ -> Attrs3
	     end,
    Attrs5 = lists:keydelete(<<"type">>, 1, Attrs4),
    Attrs6 = [{<<"type">>, <<"error">>} | Attrs5],
    Attrs6.

-spec make_error_element(binary(), binary()) -> xmlel().

make_error_element(Code, Desc) ->
    #xmlel{name = <<"error">>, attrs = [{<<"code">>, Code}],
	   children = [{xmlcdata, Desc}]}.

-spec make_correct_from_to_attrs(binary(), binary(), [attr()]) -> [attr()].

make_correct_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete(<<"from">>, 1, Attrs),
    Attrs2 = case xml:get_attr(<<"to">>, Attrs) of
	       {value, _} -> Attrs1;
	       _ -> [{<<"to">>, To} | Attrs1]
	     end,
    Attrs3 = [{<<"from">>, From} | Attrs2],
    Attrs3.

-spec replace_from_to_attrs(binary(), binary(), [attr()]) -> [attr()].

replace_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = [{<<"to">>, To} | Attrs2],
    Attrs4 = [{<<"from">>, From} | Attrs3],
    Attrs4.

-spec replace_from_to(jid(), jid(), xmlel()) -> xmlel().

replace_from_to(From, To,
		#xmlel{name = Name, attrs = Attrs, children = Els}) ->
    NewAttrs =
	replace_from_to_attrs(jlib:jid_to_string(From),
			      jlib:jid_to_string(To), Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = Els}.

-spec replace_from_attrs(binary(), [attr()]) -> [attr()].

replace_from_attrs(From, Attrs) ->
    Attrs1 = lists:keydelete(<<"from">>, 1, Attrs),
    [{<<"from">>, From} | Attrs1].

-spec replace_from(jid(), xmlel()) -> xmlel().

replace_from(From,
	     #xmlel{name = Name, attrs = Attrs, children = Els}) ->
    NewAttrs = replace_from_attrs(jlib:jid_to_string(From),
				  Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = Els}.

-spec remove_attr(binary(), xmlel()) -> xmlel().

remove_attr(Attr,
	    #xmlel{name = Name, attrs = Attrs, children = Els}) ->
    NewAttrs = lists:keydelete(Attr, 1, Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = Els}.

-spec make_jid(binary(), binary(), binary()) -> jid() | error.

make_jid(User, Server, Resource) ->
    case nodeprep(User) of
      error -> error;
      LUser ->
	  case nameprep(Server) of
	    error -> error;
	    LServer ->
		case resourceprep(Resource) of
		  error -> error;
		  LResource ->
		      #jid{user = User, server = Server, resource = Resource,
			   luser = LUser, lserver = LServer,
			   lresource = LResource}
		end
	  end
    end.

-spec make_jid({binary(), binary(), binary()}) -> jid() | error.

make_jid({User, Server, Resource}) ->
    make_jid(User, Server, Resource).

-spec string_to_jid(binary()) -> jid() | error.

string_to_jid(S) ->
    string_to_jid1(binary_to_list(S), "").

string_to_jid1([$@ | _J], "") -> error;
string_to_jid1([$@ | J], N) ->
    string_to_jid2(J, lists:reverse(N), "");
string_to_jid1([$/ | _J], "") -> error;
string_to_jid1([$/ | J], N) ->
    string_to_jid3(J, "", lists:reverse(N), "");
string_to_jid1([C | J], N) ->
    string_to_jid1(J, [C | N]);
string_to_jid1([], "") -> error;
string_to_jid1([], N) ->
    make_jid(<<"">>, list_to_binary(lists:reverse(N)), <<"">>).

%% Only one "@" is admitted per JID
string_to_jid2([$@ | _J], _N, _S) -> error;
string_to_jid2([$/ | _J], _N, "") -> error;
string_to_jid2([$/ | J], N, S) ->
    string_to_jid3(J, N, lists:reverse(S), "");
string_to_jid2([C | J], N, S) ->
    string_to_jid2(J, N, [C | S]);
string_to_jid2([], _N, "") -> error;
string_to_jid2([], N, S) ->
    make_jid(list_to_binary(N), list_to_binary(lists:reverse(S)), <<"">>).

string_to_jid3([C | J], N, S, R) ->
    string_to_jid3(J, N, S, [C | R]);
string_to_jid3([], N, S, R) ->
    make_jid(list_to_binary(N), list_to_binary(S),
             list_to_binary(lists:reverse(R))).

-spec jid_to_string(jid() | ljid()) -> binary().

jid_to_string(#jid{user = User, server = Server,
		   resource = Resource}) ->
    jid_to_string({User, Server, Resource});
jid_to_string({N, S, R}) ->
    Node = iolist_to_binary(N),
    Server = iolist_to_binary(S),
    Resource = iolist_to_binary(R),
    S1 = case Node of
	   <<"">> -> <<"">>;
	   _ -> <<Node/binary, "@">>
	 end,
    S2 = <<S1/binary, Server/binary>>,
    S3 = case Resource of
	   <<"">> -> S2;
	   _ -> <<S2/binary, "/", Resource/binary>>
	 end,
    S3.

-spec is_nodename(binary()) -> boolean().

is_nodename(Node) ->
    N = nodeprep(Node),
    (N /= error) and (N /= <<>>).

%tolower_c(C) when C >= $A, C =< $Z ->
%    C + 32;
%tolower_c(C) ->
%    C.

-define(LOWER(Char),
	if Char >= $A, Char =< $Z -> Char + 32;
	   true -> Char
	end).

%tolower(S) ->
%    lists:map(fun tolower_c/1, S).

%tolower(S) ->
%    [?LOWER(Char) || Char <- S].

-spec tolower(binary()) -> binary().

tolower(B) ->
    iolist_to_binary(tolower_s(binary_to_list(B))).

tolower_s([C | Cs]) ->
    if C >= $A, C =< $Z -> [C + 32 | tolower_s(Cs)];
       true -> [C | tolower_s(Cs)]
    end;
tolower_s([]) -> [].

%tolower([C | Cs]) when C >= $A, C =< $Z ->
%    [C + 32 | tolower(Cs)];
%tolower([C | Cs]) ->
%    [C | tolower(Cs)];
%tolower([]) ->
%    [].

-spec nodeprep(binary()) -> binary() | error.

nodeprep("") -> <<>>;
nodeprep(S) when byte_size(S) < 1024 ->
    R = stringprep:nodeprep(S),
    if byte_size(R) < 1024 -> R;
       true -> error
    end;
nodeprep(_) -> error.

-spec nameprep(binary()) -> binary() | error.

nameprep(S) when byte_size(S) < 1024 ->
    R = stringprep:nameprep(S),
    if byte_size(R) < 1024 -> R;
       true -> error
    end;
nameprep(_) -> error.

-spec resourceprep(binary()) -> binary() | error.

resourceprep(S) when byte_size(S) < 1024 ->
    R = stringprep:resourceprep(S),
    if byte_size(R) < 1024 -> R;
       true -> error
    end;
resourceprep(_) -> error.

-spec jid_tolower(jid() | ljid()) -> error | ljid().

jid_tolower(#jid{luser = U, lserver = S,
		 lresource = R}) ->
    {U, S, R};
jid_tolower({U, S, R}) ->
    case nodeprep(U) of
      error -> error;
      LUser ->
	  case nameprep(S) of
	    error -> error;
	    LServer ->
		case resourceprep(R) of
		  error -> error;
		  LResource -> {LUser, LServer, LResource}
		end
	  end
    end.

-spec jid_remove_resource(jid()) -> jid();
                         (ljid()) -> ljid().

jid_remove_resource(#jid{} = JID) ->
    JID#jid{resource = <<"">>, lresource = <<"">>};
jid_remove_resource({U, S, _R}) -> {U, S, <<"">>}.

-spec jid_replace_resource(jid(), binary()) -> error | jid().

jid_replace_resource(JID, Resource) ->
    case resourceprep(Resource) of
      error -> error;
      LResource ->
	  JID#jid{resource = Resource, lresource = LResource}
    end.

-spec get_iq_namespace(xmlel()) -> binary().

get_iq_namespace(#xmlel{name = <<"iq">>, children = Els}) ->
    case xml:remove_cdata(Els) of
        [#xmlel{attrs = Attrs}] -> xml:get_attr_s(<<"xmlns">>, Attrs);
        _                       -> <<"">>
    end;
get_iq_namespace(_) -> <<"">>.

%%
-spec(iq_query_info/1 ::
(
  Xmlel :: xmlel())
    -> iq_request() | 'reply' | 'invalid' | 'not_iq'
).

%% @spec (xmlelement()) -> iq() | reply | invalid | not_iq
iq_query_info(El) -> iq_info_internal(El, request).

%%
-spec(iq_query_or_response_info/1 ::
(
  Xmlel :: xmlel())
    -> iq_request() | iq_reply() | 'reply' | 'invalid' | 'not_iq'
).

iq_query_or_response_info(El) ->
    iq_info_internal(El, any).

iq_info_internal(#xmlel{name = <<"iq">>, attrs = Attrs, children = Els}, Filter) ->
    ID = xml:get_attr_s(<<"id">>, Attrs),
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    {Type, Class} = case xml:get_attr_s(<<"type">>, Attrs) of
        <<"set">>    -> {set,     request};
        <<"get">>    -> {get,     request};
        <<"result">> -> {result,  reply};
        <<"error">>  -> {error,   reply};
        _            -> {invalid, invalid}
    end,
    if Type == invalid -> invalid; Class == request; Filter == any ->
        FilteredEls = xml:remove_cdata(Els),
        {XMLNS, SubEl} = case {Class, FilteredEls} of
            {request, [#xmlel{attrs = Attrs2}]} ->
                {xml:get_attr_s(<<"xmlns">>, Attrs2), hd(FilteredEls)};
            {reply, _} ->
                NonErrorEls = [El || #xmlel{name = SubName} = El <- FilteredEls,
                    SubName /= <<"error">>],
                {case NonErrorEls of
                     [NonErrorEl] -> xml:get_tag_attr_s(<<"xmlns">>, NonErrorEl);
                     _            -> <<"">>
                 end,
                 FilteredEls};
            _ ->
                {<<"">>, []}
        end,
        if XMLNS == <<"">>, Class == request ->
            invalid;
        true ->
            #iq{id = ID, type = Type, xmlns = XMLNS, lang = Lang, sub_el = SubEl}
        end;
    Class == reply, Filter /= any ->
        reply
    end;
iq_info_internal(_, _) -> not_iq.

-spec is_iq_request_type(set | get | result | error) -> boolean().

is_iq_request_type(set) -> true;
is_iq_request_type(get) -> true;
is_iq_request_type(_) -> false.

iq_type_to_string(set) -> <<"set">>;
iq_type_to_string(get) -> <<"get">>;
iq_type_to_string(result) -> <<"result">>;
iq_type_to_string(error) -> <<"error">>.

-spec(iq_to_xml/1 ::
(
  IQ :: iq())
    -> xmlel()
).

iq_to_xml(#iq{id = ID, type = Type, sub_el = SubEl}) ->
    if ID /= <<"">> ->
	   #xmlel{name = <<"iq">>,
		  attrs =
		      [{<<"id">>, ID}, {<<"type">>, iq_type_to_string(Type)}],
		  children = SubEl};
       true ->
	   #xmlel{name = <<"iq">>,
		  attrs = [{<<"type">>, iq_type_to_string(Type)}],
		  children = SubEl}
    end.

-spec(parse_xdata_submit/1 ::
(
  El :: xmlel())
    -> [{Var::binary(), Values::[binary()]}]
    %%
     | 'invalid'
).

parse_xdata_submit(#xmlel{attrs = Attrs, children = Els}) ->
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"submit">> ->
            lists:reverse(parse_xdata_fields(Els, []));
        <<"form">> -> %% This is a workaround to accept Psi's wrong forms
            lists:reverse(parse_xdata_fields(Els, []));
        _ ->
            invalid
    end.

-spec(parse_xdata_fields/2 ::
(
  Xmlels :: [xmlel() | cdata()],
  Res    :: [{Var::binary(), Values :: [binary()]}])
    -> [{Var::binary(), Values::[binary()]}]
).

parse_xdata_fields([], Res) -> Res;
parse_xdata_fields([#xmlel{name = <<"field">>, attrs = Attrs, children = SubEls}
  | Els], Res) ->
    case xml:get_attr_s(<<"var">>, Attrs) of
        <<>> ->
            parse_xdata_fields(Els, Res);
        Var ->
            Field = {Var, lists:reverse(parse_xdata_values(SubEls, []))},
            parse_xdata_fields(Els, [Field | Res])
>>>>>>> upstream/master
    end;
parse_xdata_fields([_ | Els], Res) ->
    parse_xdata_fields(Els, Res).

<<<<<<< HEAD
parse_xdata_values([], Res) ->
    Res;
parse_xdata_values([#xmlel{name = 'value', children = SubEls} | Els], Res) ->
    Val = exmpp_xml:get_cdata_from_list_as_list(SubEls),
=======
-spec(parse_xdata_values/2 ::
(
  Xmlels :: [xmlel() | cdata()],
  Res    :: [binary()])
    -> [binary()]
).

parse_xdata_values([], Res) -> Res;
parse_xdata_values([#xmlel{name = <<"value">>, children = SubEls} | Els], Res) ->
    Val = xml:get_cdata(SubEls),
>>>>>>> upstream/master
    parse_xdata_values(Els, [Val | Res]);
parse_xdata_values([_ | Els], Res) ->
    parse_xdata_values(Els, Res).

<<<<<<< HEAD
rsm_decode(#iq{payload=SubEl})->
	rsm_decode(SubEl);
rsm_decode(#xmlel{}=SubEl)->
	case exmpp_xml:get_element(SubEl, 'set') of
		undefined ->
			none;
		#xmlel{name = 'set', children = SubEls}->
			lists:foldl(fun rsm_parse_element/2, #rsm_in{}, SubEls)
	end.

rsm_parse_element(#xmlel{name = 'max'}=Elem, RsmIn)->
    CountStr = exmpp_xml:get_cdata_as_list(Elem),
    {Count, _} = string:to_integer(CountStr),
    RsmIn#rsm_in{max=Count};

rsm_parse_element(#xmlel{name = 'before'}=Elem, RsmIn)->
    UID = exmpp_xml:get_cdata_as_list(Elem),
    RsmIn#rsm_in{direction=before, id=UID};

rsm_parse_element(#xmlel{name = 'after'}=Elem, RsmIn)->
    UID = exmpp_xml:get_cdata_as_list(Elem),
    RsmIn#rsm_in{direction=aft, id=UID};

rsm_parse_element(#xmlel{name = 'index'}=Elem, RsmIn)->
    IndexStr = exmpp_xml:get_cdata_as_list(Elem),
    {Index, _} = string:to_integer(IndexStr),
    RsmIn#rsm_in{index=Index};


rsm_parse_element(_, RsmIn)->
    RsmIn.

rsm_encode(#iq{payload=SubEl}=IQ_Rec,RsmOut)->
    Set = #xmlel{ns = ?NS_RSM, name = 'set', children =
	   lists:reverse(rsm_encode_out(RsmOut))},
    New = exmpp_xml:prepend_child(SubEl, Set),
    IQ_Rec#iq{payload=New}.

rsm_encode(none)->
    [];
rsm_encode(RsmOut)->
    [#xmlel{ns = ?NS_RSM, name = 'set', children = lists:reverse(rsm_encode_out(RsmOut))}].
rsm_encode_out(#rsm_out{count=Count, index=Index, first=First, last=Last})->
=======
-spec rsm_decode(iq() | xmlel()) -> none | rsm_in().

rsm_decode(#iq{sub_el = SubEl}) -> rsm_decode(SubEl);
rsm_decode(#xmlel{} = SubEl) ->
    case xml:get_subtag(SubEl, <<"set">>) of
      false -> none;
      #xmlel{name = <<"set">>, children = SubEls} ->
	  lists:foldl(fun rsm_parse_element/2, #rsm_in{}, SubEls)
    end.

rsm_parse_element(#xmlel{name = <<"max">>, attrs = []} =
		      Elem,
		  RsmIn) ->
    CountStr = xml:get_tag_cdata(Elem),
    {Count, _} = str:to_integer(CountStr),
    RsmIn#rsm_in{max = Count};
rsm_parse_element(#xmlel{name = <<"before">>,
			 attrs = []} =
		      Elem,
		  RsmIn) ->
    UID = xml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction = before, id = UID};
rsm_parse_element(#xmlel{name = <<"after">>,
			 attrs = []} =
		      Elem,
		  RsmIn) ->
    UID = xml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction = aft, id = UID};
rsm_parse_element(#xmlel{name = <<"index">>,
			 attrs = []} =
		      Elem,
		  RsmIn) ->
    IndexStr = xml:get_tag_cdata(Elem),
    {Index, _} = str:to_integer(IndexStr),
    RsmIn#rsm_in{index = Index};
rsm_parse_element(_, RsmIn) -> RsmIn.

-spec rsm_encode(iq(), rsm_out()) -> iq().

rsm_encode(#iq{sub_el = SubEl} = IQ, RsmOut) ->
    Set = #xmlel{name = <<"set">>,
		 attrs = [{<<"xmlns">>, ?NS_RSM}],
		 children = lists:reverse(rsm_encode_out(RsmOut))},
    #xmlel{name = Name, attrs = Attrs, children = SubEls} =
	SubEl,
    New = #xmlel{name = Name, attrs = Attrs,
		 children = [Set | SubEls]},
    IQ#iq{sub_el = New}.

-spec rsm_encode(none | rsm_out()) -> [xmlel()].

rsm_encode(none) -> [];
rsm_encode(RsmOut) ->
    [#xmlel{name = <<"set">>,
	    attrs = [{<<"xmlns">>, ?NS_RSM}],
	    children = lists:reverse(rsm_encode_out(RsmOut))}].

rsm_encode_out(#rsm_out{count = Count, index = Index,
			first = First, last = Last}) ->
>>>>>>> upstream/master
    El = rsm_encode_first(First, Index, []),
    El2 = rsm_encode_last(Last, El),
    rsm_encode_count(Count, El2).

rsm_encode_first(undefined, undefined, Arr) -> Arr;
rsm_encode_first(First, undefined, Arr) ->
<<<<<<< HEAD
    [#xmlel{ns = ?NS_RSM, name = 'first', children = [#xmlcdata{cdata = list_to_binary(First)}]}|Arr];
rsm_encode_first(First, Index, Arr) ->
    [#xmlel{ns = ?NS_RSM, name = 'first', attrs = [?XMLATTR(<<"index">>, Index)], children = [#xmlcdata{cdata = list_to_binary(First)}]}|Arr].

rsm_encode_last(undefined, Arr) -> Arr;
rsm_encode_last(Last, Arr) ->
    [#xmlel{ns = ?NS_RSM, name = 'last', children = [#xmlcdata{cdata = list_to_binary(Last)}]}|Arr].

rsm_encode_count(undefined, Arr)-> Arr;
rsm_encode_count(Count, Arr)->
    [#xmlel{ns = ?NS_RSM, name = 'count', children = [#xmlcdata{cdata = i2b(Count)}]} | Arr].

i2b(I) when is_integer(I) -> list_to_binary(integer_to_list(I));
i2b(L) when is_list(L)    -> list_to_binary(L).
=======
    [#xmlel{name = <<"first">>, attrs = [],
	    children = [{xmlcdata, First}]}
     | Arr];
rsm_encode_first(First, Index, Arr) ->
    [#xmlel{name = <<"first">>,
	    attrs = [{<<"index">>, i2l(Index)}],
	    children = [{xmlcdata, First}]}
     | Arr].

rsm_encode_last(undefined, Arr) -> Arr;
rsm_encode_last(Last, Arr) ->
    [#xmlel{name = <<"last">>, attrs = [],
	    children = [{xmlcdata, Last}]}
     | Arr].

rsm_encode_count(undefined, Arr) -> Arr;
rsm_encode_count(Count, Arr) ->
    [#xmlel{name = <<"count">>, attrs = [],
	    children = [{xmlcdata, i2l(Count)}]}
     | Arr].

-type tz() :: {binary(), {integer(), integer()}} | {integer(), integer()} | utc.
>>>>>>> upstream/master

%% Timezone = utc | {Sign::string(), {Hours, Minutes}} | {Hours, Minutes}
%% Hours = integer()
%% Minutes = integer()
<<<<<<< HEAD
timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}, Timezone) ->
    timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}, {milliseconds, 0}}, Timezone);
timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}, {_SubsecondUnit, SubsecondValue}}, Timezone) ->
    Timestamp_string = lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0w",
        [Year, Month, Day, Hour, Minute, Second, SubsecondValue])),
    Timezone_string = case Timezone of
	utc -> "Z";
	{Sign, {TZh, TZm}} ->
		io_lib:format("~s~2..0w:~2..0w", [Sign, TZh, TZm]);
	{TZh, TZm} -> 
		Sign = case TZh >= 0 of
			true -> "+";
			false -> "-"
		end,
		io_lib:format("~s~2..0w:~2..0w", [Sign, abs(TZh),TZm])
    end,
    {Timestamp_string, Timezone_string}.

timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).

timestamp_to_xml(DateTime, Timezone, FromJID, Desc) ->
    {T_string, Tz_string} = timestamp_to_iso(DateTime, Timezone),
    From = exmpp_jid:to_list(FromJID),
    P1 = exmpp_xml:set_attributes(#xmlel{ns = ?NS_DELAY, name = 'delay'},
      [{<<"from">>, From},
       {<<"stamp">>, T_string ++ Tz_string}]),
    exmpp_xml:set_cdata(P1, Desc).

%% TODO: Remove this function once XEP-0091 is Obsolete
timestamp_to_xml({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    Timestamp = lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
	[Year, Month, Day, Hour, Minute, Second])),
    exmpp_xml:set_attribute(#xmlel{ns = ?NS_DELAY_OLD, name = 'x'},
      <<"stamp">>, Timestamp).
=======
-spec timestamp_to_iso(calendar:datetime(), tz()) -> {binary(), binary()}.

timestamp_to_iso({{Year, Month, Day},
                  {Hour, Minute, Second}},
                 Timezone) ->
    Timestamp_string =
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
				    [Year, Month, Day, Hour, Minute, Second])),
    Timezone_string = case Timezone of
			utc -> "Z";
			{Sign, {TZh, TZm}} ->
			    io_lib:format("~s~2..0w:~2..0w", [Sign, TZh, TZm]);
			{TZh, TZm} ->
			    Sign = case TZh >= 0 of
				     true -> "+";
				     false -> "-"
				   end,
			    io_lib:format("~s~2..0w:~2..0w",
					  [Sign, abs(TZh), TZm])
		      end,
    {iolist_to_binary(Timestamp_string), iolist_to_binary(Timezone_string)}.

-spec timestamp_to_iso(calendar:datetime()) -> binary().

timestamp_to_iso({{Year, Month, Day},
                  {Hour, Minute, Second}}) ->
    iolist_to_binary(io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
                                   [Year, Month, Day, Hour, Minute, Second])).

-spec timestamp_to_xml(calendar:datetime(), tz(), jid(), binary()) -> xmlel().

timestamp_to_xml(DateTime, Timezone, FromJID, Desc) ->
    {T_string, Tz_string} = timestamp_to_iso(DateTime,
					     Timezone),
    Text = [{xmlcdata, Desc}],
    From = jlib:jid_to_string(FromJID),
%% TODO: Remove this function once XEP-0091 is Obsolete
    #xmlel{name = <<"delay">>,
	   attrs =
	       [{<<"xmlns">>, ?NS_DELAY}, {<<"from">>, From},
		{<<"stamp">>, <<T_string/binary, Tz_string/binary>>}],
	   children = Text}.

-spec timestamp_to_xml(calendar:datetime()) -> xmlel().

timestamp_to_xml({{Year, Month, Day},
		  {Hour, Minute, Second}}) ->
    #xmlel{name = <<"x">>,
	   attrs =
	       [{<<"xmlns">>, ?NS_DELAY91},
		{<<"stamp">>,
		 iolist_to_binary(io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
						[Year, Month, Day, Hour, Minute,
						 Second]))}],
	   children = []}.

-spec now_to_utc_string(erlang:timestamp()) -> binary().
>>>>>>> upstream/master

now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:now_to_universal_time({MegaSecs, Secs,
					MicroSecs}),
    list_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6."
                                 ".0wZ",
                                 [Year, Month, Day, Hour, Minute, Second,
                                  MicroSecs])).

-spec now_to_local_string(erlang:timestamp()) -> binary().

now_to_local_string({MegaSecs, Secs, MicroSecs}) ->
    LocalTime = calendar:now_to_local_time({MegaSecs, Secs,
					    MicroSecs}),
    UTCTime = calendar:now_to_universal_time({MegaSecs,
					      Secs, MicroSecs}),
    Seconds =
	calendar:datetime_to_gregorian_seconds(LocalTime) -
	  calendar:datetime_to_gregorian_seconds(UTCTime),
    {{H, M, _}, Sign} = if Seconds < 0 ->
			       {calendar:seconds_to_time(-Seconds), "-"};
			   true -> {calendar:seconds_to_time(Seconds), "+"}
			end,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	LocalTime,
    list_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6."
                                 ".0w~s~2..0w:~2..0w",
                                 [Year, Month, Day, Hour, Minute, Second,
                                  MicroSecs, Sign, H, M])).

-spec datetime_string_to_timestamp(binary()) -> undefined | erlang:timestamp().

<<<<<<< HEAD
% {yyyy-mm-dd|yyyymmdd}Thh:mm:ss[.sss]{|Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs} | undefined
=======
>>>>>>> upstream/master
datetime_string_to_timestamp(TimeStr) ->
    case catch parse_datetime(TimeStr) of
      {'EXIT', _Err} -> undefined;
      TimeStamp -> TimeStamp
    end.

parse_datetime(TimeStr) ->
    [Date, Time] = str:tokens(TimeStr, <<"T">>),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1,
						  1},
						 {0, 0, 0}}),
    Seconds = S - S1 - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

% yyyy-mm-dd | yyyymmdd
parse_date(Date) ->
<<<<<<< HEAD
    {Y, M, D} =
	case string:tokens(Date, "-") of
	    [Y1, M1, D1] -> {Y1, M1, D1};
	    [[Y1, Y2, Y3, Y4, M1, M2, D1, D2]] -> {[Y1, Y2, Y3, Y4], [M1, M2], [D1, D2]}
	end,
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
=======
    [Y, M, D] = str:tokens(Date, <<"-">>),
    Date1 = {binary_to_integer(Y), binary_to_integer(M),
	     binary_to_integer(D)},
>>>>>>> upstream/master
    case calendar:valid_date(Date1) of
      true -> Date1;
      _ -> false
    end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case str:str(Time, <<"Z">>) of
      0 -> parse_time_with_timezone(Time);
      _ ->
	  [T | _] = str:tokens(Time, <<"Z">>),
	  {TT, MS} = parse_time1(T),
	  {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
<<<<<<< HEAD
    case string:str(Time, "+") of
	0 ->
	    case string:str(Time, "-") of
		0 ->
		    {TT, MS} = parse_time1(Time),
		    {TT, MS, 0, 0};
		_ ->
		    parse_time_with_timezone(Time, "-")
	    end;
	_ ->
	    parse_time_with_timezone(Time, "+")
=======
    case str:str(Time, <<"+">>) of
      0 ->
	  case str:str(Time, <<"-">>) of
	    0 -> false;
	    _ -> parse_time_with_timezone(Time, <<"-">>)
	  end;
      _ -> parse_time_with_timezone(Time, <<"+">>)
>>>>>>> upstream/master
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = str:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
      <<"-">> -> {TT, MS, -TZH, -TZM};
      <<"+">> -> {TT, MS, TZH, TZM}
    end.

parse_timezone(TZ) ->
    [H, M] = str:tokens(TZ, <<":">>),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] = str:tokens(Time, <<".">>),
    MS = case T of
	   [] -> 0;
	   [Val] -> binary_to_integer(str:left(Val, 6, $0))
	 end,
    [H, M, S] = str:tokens(HMS, <<":">>),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60},
				       {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(fun ({L, N}, B) ->
			   V = binary_to_integer(L),
			   if (V >= 0) and (V =< N) -> {V, B};
			      true -> {false, false}
			   end
		   end,
		   true, List).

%
% Base64 stuff (based on httpd_util.erl)
%

-spec term_to_base64(term()) -> binary().

term_to_base64(Term) ->
    encode_base64(term_to_binary(Term)).

-spec base64_to_term(binary()) -> {term, term()} | error.

base64_to_term(Base64) ->
    case catch binary_to_term(decode_base64(Base64), [safe]) of
      {'EXIT', _} ->
	  error;
      Term ->
	  {term, Term}
    end.

-spec decode_base64(binary()) -> binary().

decode_base64(S) ->
    decode_base64_bin(S, <<>>).

take_without_spaces(Bin, Count) -> 
    take_without_spaces(Bin, Count, <<>>).

take_without_spaces(Bin, 0, Acc) ->
    {Acc, Bin};
take_without_spaces(<<>>, _, Acc) ->
    {Acc, <<>>};
take_without_spaces(<<$\s, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\t, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\n, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\r, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<Char:8, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count-1, <<Acc/binary, Char:8>>).

decode_base64_bin(<<>>, Acc) ->
    Acc;
decode_base64_bin(Bin, Acc) ->
    case take_without_spaces(Bin, 4) of
        {<<A, B, $=, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B) bsr 4):2>>;
        {<<A, B, C, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B)):6, (d(C) bsr 2):4>>;
        {<<A, B, C, D>>, Tail} ->
            Acc2 = <<Acc/binary, (d(A)):6, (d(B)):6, (d(C)):6, (d(D)):6>>,
            decode_base64_bin(Tail, Acc2);
        _ ->
            <<"">>
    end.

d(X) when X >= $A, X =< $Z -> X - 65;
d(X) when X >= $a, X =< $z -> X - 71;
d(X) when X >= $0, X =< $9 -> X + 4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.


<<<<<<< HEAD
encode_base64([]) ->
    [];
encode_base64([A]) ->
    [e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A,B]) ->
    [e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)), e((B band 15) bsl 2), $=];
encode_base64([A,B,C|Ls]) ->
    encode_base64_do(A,B,C, Ls).
encode_base64_do(A,B,C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [e(BB bsr 18), e((BB bsr 12) band 63), 
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].

e(X) when X >= 0, X < 26 -> X+65;
e(X) when X>25, X<52 ->     X+71;
e(X) when X>51, X<62 ->     X-4;
e(62) ->                    $+;
e(63) ->                    $/;
e(X) ->                     exit({bad_encode_base64_token, X}).

%% @doc Deprecated for {@link inet_parse:ntoa/1}.
%% ```
%% - jlib:ip_to_list
%% + inet_parse:ntoa(IpTuple)
%% '''
=======
>>>>>>> upstream/master
%% Convert Erlang inet IP to list
-spec encode_base64(binary()) -> binary().

encode_base64(Data) ->
    encode_base64_bin(Data, <<>>).

encode_base64_bin(<<A:6, B:6, C:6, D:6, Tail/binary>>, Acc) ->
    encode_base64_bin(Tail, <<Acc/binary, (e(A)):8, (e(B)):8, (e(C)):8, (e(D)):8>>);
encode_base64_bin(<<A:6, B:6, C:4>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B)):8, (e(C bsl 2)):8, $=>>;
encode_base64_bin(<<A:6, B:2>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B bsl 4)):8, $=, $=>>;
encode_base64_bin(<<>>, Acc) ->
    Acc.

e(X) when X >= 0, X < 26 -> X + 65;
e(X) when X > 25, X < 52 -> X + 71;
e(X) when X > 51, X < 62 -> X - 4;
e(62) -> $+;
e(63) -> $/;
e(X) -> exit({bad_encode_base64_token, X}).

-spec ip_to_list(inet:ip_address() | undefined |
                 {inet:ip_address(), inet:port_number()}) -> binary().

ip_to_list({IP, _Port}) ->
    ip_to_list(IP);
<<<<<<< HEAD
ip_to_list(IpTuple) when is_tuple(IpTuple) ->
    inet_parse:ntoa(IpTuple);
ip_to_list(IP) ->
    lists:flatten(io_lib:format("~w", [IP])).

% --------------------------------------------------------------------
% Compat layer.
% --------------------------------------------------------------------

%% @spec (JID) -> New_JID
%%     JID = jid()
%%     New_JID = jid()
%% @doc Convert a JID from its ejabberd form to its exmpp form.
%%
%% Empty fields are set to `undefined', not the empty string.

%%TODO: this doesn't make sence!, it is still used?.
from_old_jid({jid, NodeRaw, DomainRaw, ResourceRaw, _, _, _}) ->
    Node = exmpp_stringprep:nodeprep(NodeRaw),
    Domain = exmpp_stringprep:resourceprep(DomainRaw),
    Resource = exmpp_stringprep:nameprep(ResourceRaw),
    exmpp_jid:make(Node,Domain,Resource).


short_jid(JID) ->
    {exmpp_jid:node(JID), exmpp_jid:domain(JID), exmpp_jid:resource(JID)}.

short_bare_jid(JID) ->
    short_jid(exmpp_jid:bare(JID)).

short_prepd_jid(JID) ->
    {exmpp_jid:prep_node(JID), 
     exmpp_jid:prep_domain(JID), 
     exmpp_jid:prep_resource(JID)}.

short_prepd_bare_jid(JID) ->
    short_prepd_jid(exmpp_jid:bare(JID)).


make_result_iq_reply({xmlelement, Name, Attrs, SubTags}) ->
    NewAttrs = make_result_iq_reply_attrs(Attrs),
    {xmlelement, Name, NewAttrs, SubTags}.

make_result_iq_reply_attrs(Attrs) ->
    To = xml:get_attr("to", Attrs),
    From = xml:get_attr("from", Attrs),
    Attrs1 = lists:keydelete("to", 1, Attrs),
    Attrs2 = lists:keydelete("from", 1, Attrs1),
    Attrs3 = case To of
		 {value, ToVal} ->
		     [{"from", ToVal} | Attrs2];
		 _ ->
		     Attrs2
	     end,
    Attrs4 = case From of
		 {value, FromVal} ->
		     [{"to", FromVal} | Attrs3];
		 _ ->
		     Attrs3
	     end,
    Attrs5 = lists:keydelete("type", 1, Attrs4),
    Attrs6 = [{"type", "result"} | Attrs5],
    Attrs6.

make_error_reply({xmlelement, Name, Attrs, SubTags}, Code, Desc) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    {xmlelement, Name, NewAttrs, SubTags ++ [{xmlelement, "error",
					      [{"code", Code}],
					      [{xmlcdata, Desc}]}]}.

%% @doc Deprecated for {@link exmpp_iq:error/2},
%% {@link exmpp_iq:error_without_original/2}.
%% ```
%% - jlib:make_error_reply(Packet, ?ERR_FEATURE_NOT_IMPLEMENTED)
%% + exmpp_iq:error(Packet, 'feature-not-implemented')
%% '''
%% ```
%% - jlib:make_error_reply(El, ?ERR_JID_MALFORMED)
%% + exmpp_iq:error_without_original(El, 'jid-malformed')
%% '''
%% ```
%% - jlib:make_error_reply(El, ?ERR_AUTH_NO_RESOURCE_PROVIDED("en"))
%% + exmpp_iq:error(El, exmpp_stanza:error(Namespace, 'not-acceptable', {"en", "No resource provided"}))
%% '''

make_error_reply({xmlelement, Name, Attrs, SubTags}, Error) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    {xmlelement, Name, NewAttrs, SubTags ++ [Error]}.

make_error_reply_attrs(Attrs) ->
    To = xml:get_attr("to", Attrs),
    From = xml:get_attr("from", Attrs),
    Attrs1 = lists:keydelete("to", 1, Attrs),
    Attrs2 = lists:keydelete("from", 1, Attrs1),
    Attrs3 = case To of
		 {value, ToVal} ->
		     [{"from", ToVal} | Attrs2];
		 _ ->
		     Attrs2
	     end,
    Attrs4 = case From of
		 {value, FromVal} ->
		     [{"to", FromVal} | Attrs3];
		 _ ->
		     Attrs3
	     end,
    Attrs5 = lists:keydelete("type", 1, Attrs4),
    Attrs6 = [{"type", "error"} | Attrs5],
    Attrs6.

make_error_element(Code, Desc) ->
    {xmlelement, "error",
     [{"code", Code}],
     [{xmlcdata, Desc}]}.

make_correct_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete("from", 1, Attrs),
    Attrs2 = case xml:get_attr("to", Attrs) of
		 {value, _} ->
		     Attrs1;
		 _ ->
		     [{"to", To} | Attrs1]
	     end,
    Attrs3 = [{"from", From} | Attrs2],
    Attrs3.

%% @doc Deprecated for {@link exmpp_stanza:set_recipient_in_attrs/2}.
%% ```
%% - jlib:replace_from_to_attrs(String1, String2, Attrs)
%% + exmpp_stanza:set_recipient_in_attrs(exmpp_stanza:set_sender_in_attrs(Attrs, String1), String2)
%% '''

replace_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete("to", 1, Attrs),
    Attrs2 = lists:keydelete("from", 1, Attrs1),
    Attrs3 = [{"to", To} | Attrs2],
    Attrs4 = [{"from", From} | Attrs3],
    Attrs4.

%% @doc Deprecated for {@link exmpp_stanza:set_recipient/2}.
%% ```
%% - jlib:replace_from_to(JID1, JID2, Stanza)
%% + exmpp_stanza:set_recipient(exmpp_stanza:set_sender(Stanza, JID1), JID2)
%% '''

replace_from_to(From, To, {xmlelement, Name, Attrs, Els}) ->
    NewAttrs = replace_from_to_attrs(jlib:jid_to_string(From),
				     jlib:jid_to_string(To),
				     Attrs),
    {xmlelement, Name, NewAttrs, Els}.

replace_from_attrs(From, Attrs) ->
    Attrs1 = lists:keydelete("from", 1, Attrs),
    [{"from", From} | Attrs1].

replace_from(From, {xmlelement, Name, Attrs, Els}) ->
    NewAttrs = replace_from_attrs(jlib:jid_to_string(From), Attrs),
    {xmlelement, Name, NewAttrs, Els}.

%% @doc Deprecated for {@link exmpp_stanza:remove_recipient/1}.
%% ```
%% - jlib:remove_attr("to", Stanza)
%% + exmpp_stanza:remove_recipient(Stanza)
%% '''

remove_attr(Attr, {xmlelement, Name, Attrs, Els}) ->
    NewAttrs = lists:keydelete(Attr, 1, Attrs),
    {xmlelement, Name, NewAttrs, Els}.

%% @doc Deprecated for {@link exmpp_jid:make/3}.
%% ```
%% - jlib:make_jid({Username, Server, Resource})
%% + exmpp_jid:make(Username, Server, Resource)
%% '''

make_jid({U, S, R}) ->
    make({U, S, R}).

%% @doc Deprecated for {@link exmpp_jid:make/3}.
%% ```
%% - jlib:make_jid(Username, Server, Resource)
%% + exmpp_jid:make(Username, Server, Resource)
%% '''
%% ```
%% - jlib:make_jid(Username, Server, "")
%% + exmpp_jid:bare(JID)
%% '''

make_jid(U, S, R) ->
    make(U, S, R).

make(User, Server, Resource) ->
    try
	exmpp_jid:make(User, Server, Resource)
    catch
	_Exception ->
	    error
    end.

make({User, Server, Resource}) ->
    make(User, Server, Resource).

%% @doc Deprecated for {@link exmpp_jid:parse/1}.
%% ```
%% - jlib:string_to_jid(String)
%% + exmpp_jid:parse(String)
%% '''

string_to_jid(String) ->
    exmpp_jid:parse(String).

%% @doc Deprecated for {@link exmpp_jid:to_list/1}.
%% ```
%% - jlib:jid_to_string({Node, Server, Resource}
%% + exmpp_jid:to_list(exmpp_jid:make(Node, Server, Resource))
%% '''
%% ```
%% - jlib:jid_to_string(JID)
%% + exmpp_jid:to_list(JID)
%% '''

jid_to_string({Node, Server, Resource}) ->
    Jid = exmpp_jid:make(Node, Server, Resource),
    exmpp_jid:to_list(Jid);
jid_to_string(Jid) ->
    exmpp_jid:to_list(Jid).


%% @doc Deprecated for {@link exmpp_stringprep:is_node/1}.
%% ```
%% - jlib:is_nodename(Username)
%% + exmpp_stringprep:is_node(Username)
%% '''

is_nodename(Username) ->
    exmpp_stringprep:is_node(Username).

%% @doc Deprecated for {@link exmpp_stringprep:to_lower/1}.
%% ```
%% - jlib:tolower(String)
%% + exmpp_stringprep:to_lower(String)
%% '''

%% Not tail-recursive but it seems works faster than variants above
tolower(String) ->
    exmpp_stringprep:to_lower(String).

%% @doc Deprecated for {@link exmpp_stringprep:nodeprep/1}.
%% ```
%% - jlib:nodeprep(Username)
%% + exmpp_stringprep:nodeprep(Username)
%% '''

nodeprep(Username) ->
    exmpp_stringprep:nodeprep(Username).

%% @doc Deprecated for {@link exmpp_stringprep:nameprep/1}.
%% ```
%% - jlib:nameprep(Server)
%% + exmpp_stringprep:nameprep(Server)
%% '''

nameprep(Server) ->
    exmpp_stringprep:nameprep(Server).

%% @doc Deprecated for {@link exmpp_stringprep:resourceprep/1}.
%% ```
%% - jlib:resourceprep(Resource)
%% + exmpp_stringprep:resourceprep(Resource)
%% '''

resourceprep(Resource) ->
    exmpp_stringprep:resourceprep(Resource).

%% @doc Deprecated for {@link jlib:short_prepd_jid/1}.
%% ```
%% - jlib:jid_tolower(JID)
%% + jlib:short_prepd_jid(JID)
%% '''
%% ```
%% - jlib:jid_tolower(JID)
%% +  {exmpp_jid:prep_node_as_list(JID), exmpp_jid:prep_domain_as_list(JID), exmpp_jid:prep_resource_as_list(JID)}
%% '''

jid_tolower({U, S, R}) ->
    jid_tolower(exmpp_jid:make(U, S, R));
jid_tolower(JID) ->
    jlib:short_prepd_jid(JID).

%% @doc Deprecated for {@link jlib:short_prepd_bare_jid/1}.
%% ```
%% - jlib:jid_remove_resource(jlib:jid_tolower(String))
%% + jlib:short_prepd_bare_jid(String)
%% '''

jid_remove_resource({U, S, R}) ->
    short_prepd_bare_jid(exmpp_jid:make(U, S, R));
jid_remove_resource(JID) ->
    short_prepd_bare_jid(JID).

%% @doc Deprecated for {@link exmpp_jid:full/2}.
%% ```
%% - jlib:jid_replace_resource(JID, R)
%% + exmpp_jid:full(JID, R)
%% '''

jid_replace_resource(JID, Resource) ->
    exmpp_jid:full(JID, Resource).


%% @deprecated
get_iq_namespace({xmlelement, Name, _Attrs, Els}) when Name == "iq" ->
    case xml:remove_cdata(Els) of
	[{xmlelement, _Name2, Attrs2, _Els2}] ->
	    xml:get_attr_s("xmlns", Attrs2);
	_ ->
	    ""
    end;
get_iq_namespace(_) ->
    "".

%% @doc Deprecated for {@link exmpp_iq:xmlel_to_iq/1}.
%% ```
%% - jlib:iq_query_info(Packet)
%% + exmpp_iq:xmlel_to_iq(Packet)
%% '''

iq_query_info(El) ->
    exmpp_iq:xmlel_to_iq(El).

iq_query_or_response_info(El) ->
    exmpp_iq:xmlel_to_iq(El).

is_iq_request_type(set) -> true;
is_iq_request_type(get) -> true;
is_iq_request_type(_) -> false.

%% @doc Deprecated for {@link exmpp_iq:iq_to_xmlel/1}.
%% ```
%% - jlib:iq_to_xml(IQ)
%% + exmpp_iq:iq_to_xmlel(IQ)
%% '''

iq_to_xml(IQ) ->
    exmpp_iq:iq_to_xmlel(IQ).
=======
%% This function clause could use inet_parse too:
ip_to_list(undefined) ->
    <<"unknown">>;
ip_to_list(IP) ->
    list_to_binary(inet_parse:ntoa(IP)).

binary_to_atom(Bin) ->
    erlang:binary_to_atom(Bin, utf8).

binary_to_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

binary_to_integer(Bin, Base) ->
    list_to_integer(binary_to_list(Bin), Base).

integer_to_binary(I) ->
    list_to_binary(integer_to_list(I)).

integer_to_binary(I, Base) ->
    list_to_binary(erlang:integer_to_list(I, Base)).

tuple_to_binary(T) ->
    iolist_to_binary(tuple_to_list(T)).

atom_to_binary(A) ->
    erlang:atom_to_binary(A, utf8).


l2i(I) when is_integer(I) -> I;
l2i(L) when is_binary(L) -> binary_to_integer(L).

i2l(I) when is_integer(I) -> integer_to_binary(I);
i2l(L) when is_binary(L) -> L.

i2l(I, N) when is_integer(I) -> i2l(i2l(I), N);
i2l(L, N) when is_binary(L) ->
    case str:len(L) of
      N -> L;
      C when C > N -> L;
      _ -> i2l(<<$0, L/binary>>, N)
    end.

-spec queue_drop_while(fun((term()) -> boolean()), queue()) -> queue().

queue_drop_while(F, Q) ->
    case queue:peek(Q) of
      {value, Item} ->
	  case F(Item) of
	    true ->
		queue_drop_while(F, queue:drop(Q));
	    _ ->
		Q
	  end;
      empty ->
	  Q
    end.
>>>>>>> upstream/master
