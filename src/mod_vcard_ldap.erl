%%%----------------------------------------------------------------------
%%% File    : mod_vcard_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for VCards from LDAP storage.
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_vcard_ldap).

-author('alexey@process-one.net').

-behaviour(gen_server).

-behaviour(gen_mod).

%% gen_server callbacks.
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-export([start/2, start_link/2, stop/1,
	 get_sm_features/5, process_local_iq/3, process_sm_iq/3,
	 remove_user/1, route/4, transform_module_options/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD
-include("eldap/eldap.hrl").

-define(PROCNAME, ejabberd_mod_vcard_ldap).

-record(state, {serverhost,
		myhost,
		eldap_id,
		search,
		servers,
		backups,
		port,
		tls_options,
		dn,
		base,
		password,
		uids,
		vcard_map,
		vcard_map_attrs,
		user_filter,
		search_filter,
		search_fields,
		search_reported,
		search_reported_attrs,
                deref_aliases,
		matches
	       }).
=======
-include("logger.hrl").

-include("eldap.hrl").

-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_vcard_ldap).

-record(state,
	{serverhost = <<"">>        :: binary(),
         myhost = <<"">>            :: binary(),
         eldap_id = <<"">>          :: binary(),
         search = true              :: boolean(),
         servers = []               :: [binary()],
         backups = []               :: [binary()],
	 port = ?LDAP_PORT          :: inet:port_number(),
         tls_options = []           :: list(),
         dn = <<"">>                :: binary(),
         base = <<"">>              :: binary(),
         password = <<"">>          :: binary(),
         uids = []                  :: [{binary()} | {binary(), binary()}],
         vcard_map = []             :: [{binary(), binary(), [binary()]}],
	 vcard_map_attrs = []       :: [binary()],
         user_filter = <<"">>       :: binary(),
         search_filter              :: eldap:filter(),
	 search_fields = []         :: [{binary(), binary()}],
         search_reported = []       :: [{binary(), binary()}],
         search_reported_attrs = [] :: [binary()],
	 deref_aliases = never      :: never | searching | finding | always,
         matches = 0                :: non_neg_integer()}).
>>>>>>> upstream/master

-define(VCARD_MAP,
	[{<<"NICKNAME">>, <<"%u">>, []},
	 {<<"FN">>, <<"%s">>, [<<"displayName">>]},
	 {<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
	 {<<"GIVEN">>, <<"%s">>, [<<"givenName">>]},
	 {<<"MIDDLE">>, <<"%s">>, [<<"initials">>]},
	 {<<"ORGNAME">>, <<"%s">>, [<<"o">>]},
	 {<<"ORGUNIT">>, <<"%s">>, [<<"ou">>]},
	 {<<"CTRY">>, <<"%s">>, [<<"c">>]},
	 {<<"LOCALITY">>, <<"%s">>, [<<"l">>]},
	 {<<"STREET">>, <<"%s">>, [<<"street">>]},
	 {<<"REGION">>, <<"%s">>, [<<"st">>]},
	 {<<"PCODE">>, <<"%s">>, [<<"postalCode">>]},
	 {<<"TITLE">>, <<"%s">>, [<<"title">>]},
	 {<<"URL">>, <<"%s">>, [<<"labeleduri">>]},
	 {<<"DESC">>, <<"%s">>, [<<"description">>]},
	 {<<"TEL">>, <<"%s">>, [<<"telephoneNumber">>]},
	 {<<"EMAIL">>, <<"%s">>, [<<"mail">>]},
	 {<<"BDAY">>, <<"%s">>, [<<"birthDay">>]},
	 {<<"ROLE">>, <<"%s">>, [<<"employeeType">>]},
	 {<<"PHOTO">>, <<"%s">>, [<<"jpegPhoto">>]}]).

-define(SEARCH_FIELDS,
	[{<<"User">>, <<"%u">>},
	 {<<"Full Name">>, <<"displayName">>},
	 {<<"Given Name">>, <<"givenName">>},
	 {<<"Middle Name">>, <<"initials">>},
	 {<<"Family Name">>, <<"sn">>},
	 {<<"Nickname">>, <<"%u">>},
	 {<<"Birthday">>, <<"birthDay">>},
	 {<<"Country">>, <<"c">>}, {<<"City">>, <<"l">>},
	 {<<"Email">>, <<"mail">>},
	 {<<"Organization Name">>, <<"o">>},
	 {<<"Organization Unit">>, <<"ou">>}]).

-define(SEARCH_REPORTED,
	[{<<"Full Name">>, <<"FN">>},
	 {<<"Given Name">>, <<"FIRST">>},
	 {<<"Middle Name">>, <<"MIDDLE">>},
	 {<<"Family Name">>, <<"LAST">>},
	 {<<"Nickname">>, <<"NICK">>},
	 {<<"Birthday">>, <<"BDAY">>},
	 {<<"Country">>, <<"CTRY">>},
	 {<<"City">>, <<"LOCALITY">>},
	 {<<"Email">>, <<"EMAIL">>},
	 {<<"Organization Name">>, <<"ORGNAME">>},
	 {<<"Organization Unit">>, <<"ORGUNIT">>}]).

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

terminate(_Reason, State) ->
    Host = State#state.serverhost,
<<<<<<< HEAD
    HostB = list_to_binary(Host),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 50),
=======
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 50),
>>>>>>> upstream/master
    case State#state.search of
      true ->
	  ejabberd_router:unregister_route(State#state.myhost);
      _ -> ok
    end.

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

init([Host, Opts]) ->
    HostB = list_to_binary(Host),
    State = parse_options(Host, Opts),
<<<<<<< HEAD
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, 
            list_to_binary(Host), ?MODULE, get_sm_features, 50),
    eldap_pool:start_link(State#state.eldap_id,
		     State#state.servers,
		     State#state.backups,
		     State#state.port,
		     State#state.dn,
		     State#state.password,
		     State#state.tls_options),
=======
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_VCARD, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_VCARD, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 50),
    eldap_pool:start_link(State#state.eldap_id,
			  State#state.servers, State#state.backups,
			  State#state.port, State#state.dn,
			  State#state.password, State#state.tls_options),
>>>>>>> upstream/master
    case State#state.search of
      true ->
	  ejabberd_router:register_route(State#state.myhost);
      _ -> ok
    end,
    {ok, State}.

handle_info({route, From, To, Packet}, State) ->
    case catch do_route(State, From, To, Packet) of
<<<<<<< HEAD
	Pid when is_pid(Pid) ->
	    ok;
	_ ->
	    Err = exmpp_stanza:reply_with_error(Packet,
              'internal-server-error'),
	    ejabberd_router:route(To, From, Err)
=======
      Pid when is_pid(Pid) -> ok;
      _ ->
	  Err = jlib:make_error_reply(Packet,
				      ?ERR_INTERNAL_SERVER_ERROR),
	  ejabberd_router:route(To, From, Err)
>>>>>>> upstream/master
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

get_sm_features({error, _Error} = Acc, _From, _To,
		_Node, _Lang) ->
    Acc;
get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
<<<<<<< HEAD
	[] ->
	    case Acc of
		{result, Features} ->
		    {result, [?NS_VCARD_s | Features]};
		empty ->
		    {result, [?NS_VCARD_s]}
	    end;
	_ ->
	    Acc
    end.

process_local_iq(_From, _To, #iq{type = get, lang = Lang} = IQ_Rec) ->
    Result = #xmlel{ns = ?NS_VCARD, name = 'vCard', children = [
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'FN'},
	  "ejabberd"),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'URL'},
	  ?EJABBERD_URI),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'DESC'},
	  translate:translate(Lang, "Erlang Jabber Server") ++
	  "\nCopyright (c) 2002-2012 ProcessOne"),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'BDAY'},
	  "2002-11-16")
      ]},
    exmpp_iq:result(IQ_Rec, Result);
process_local_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

process_sm_iq(_From, To, #iq{} = IQ_Rec) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    case catch process_vcard_ldap(To, IQ_Rec, LServer) of
	{'EXIT', _} ->
            exmpp_iq:error(IQ_Rec, 'internal-server-error');
	Other ->
	    Other
=======
      <<"">> ->
	  case Acc of
	    {result, Features} -> {result, [?NS_VCARD | Features]};
	    empty -> {result, [?NS_VCARD]}
	  end;
      _ -> Acc
    end.

process_local_iq(_From, _To,
		 #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"vCard">>,
			    attrs = [{<<"xmlns">>, ?NS_VCARD}],
			    children =
				[#xmlel{name = <<"FN">>, attrs = [],
					children =
					    [{xmlcdata, <<"ejabberd">>}]},
				 #xmlel{name = <<"URL">>, attrs = [],
					children = [{xmlcdata, ?EJABBERD_URI}]},
				 #xmlel{name = <<"DESC">>, attrs = [],
					children =
					    [{xmlcdata,
					      <<(translate:translate(Lang,
								     <<"Erlang Jabber Server">>))/binary,
						"\nCopyright (c) 2002-2014 ProcessOne">>}]},
				 #xmlel{name = <<"BDAY">>, attrs = [],
					children =
					    [{xmlcdata, <<"2002-11-16">>}]}]}]}
    end.

process_sm_iq(_From, #jid{lserver = LServer} = To,
	      #iq{sub_el = SubEl} = IQ) ->
    case catch process_vcard_ldap(To, IQ, LServer) of
      {'EXIT', _} ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
      Other -> Other
>>>>>>> upstream/master
    end.

process_vcard_ldap(To, IQ_Rec, Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?PROCNAME),
<<<<<<< HEAD
    case IQ_Rec#iq.type of
	set ->
            exmpp_iq:error(IQ_Rec, 'not-allowed');
	get ->
        LUser = exmpp_jid:prep_node_as_list(To),
	    LServer = State#state.serverhost,
	    case ejabberd_auth:is_user_exists(LUser, LServer) of
		true ->
		    VCardMap = State#state.vcard_map,
		    case find_ldap_user(LUser, State) of
			#eldap_entry{attributes = Attributes} ->
			    VcardEl = ldap_attributes_to_vcard(Attributes, VCardMap, {LUser, LServer}),
                            exmpp_iq:result(IQ_Rec, VcardEl);
			_ ->
                            exmpp_iq:result(IQ_Rec)
		    end;
		_ ->
                    exmpp_iq:result(IQ_Rec)
	    end
	end.
=======
    #iq{type = Type, sub_el = SubEl} = IQ,
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  #jid{luser = LUser} = To,
	  LServer = State#state.serverhost,
	  case ejabberd_auth:is_user_exists(LUser, LServer) of
	    true ->
		VCardMap = State#state.vcard_map,
		case find_ldap_user(LUser, State) of
		  #eldap_entry{attributes = Attributes} ->
		      Vcard = ldap_attributes_to_vcard(Attributes, VCardMap,
						       {LUser, LServer}),
		      IQ#iq{type = result, sub_el = Vcard};
		  _ -> IQ#iq{type = result, sub_el = []}
		end;
	    _ -> IQ#iq{type = result, sub_el = []}
	  end
    end.
>>>>>>> upstream/master

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

find_ldap_user(User, State) ->
    Base = State#state.base,
    RFC2254_Filter = State#state.user_filter,
    Eldap_ID = State#state.eldap_id,
    VCardAttrs = State#state.vcard_map_attrs,
<<<<<<< HEAD
    case eldap_filter:parse(RFC2254_Filter, [{"%u", User}]) of
	{ok, EldapFilter} ->
	    case eldap_pool:search(Eldap_ID,
                                   [{base, Base},
                                    {filter, EldapFilter},
                                    {deref_aliases, State#state.deref_aliases},
                                    {attributes, VCardAttrs}]) of
		#eldap_search_result{entries = [E | _]} ->
		    E;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

ldap_attributes_to_vcard(Attributes, VCardMap, UD) ->
    Attrs = lists:map(
	      fun({VCardName, _, _}) ->
		      {exmpp_stringprep:to_lower(VCardName),
		       map_vcard_attr(VCardName, Attributes, VCardMap, UD)}
	      end, VCardMap),
    Elts = [ldap_attribute_to_vcard(vCard, Attr) || Attr <- Attrs],
    NElts = [ldap_attribute_to_vcard(vCardN, Attr) || Attr <- Attrs],
    OElts = [ldap_attribute_to_vcard(vCardO, Attr) || Attr <- Attrs],
    AElts = [ldap_attribute_to_vcard(vCardA, Attr) || Attr <- Attrs],
    #xmlel{ns = ?NS_VCARD, name = 'vCard', children =
      lists:append([X || X <- Elts, X /= none],
		   [#xmlel{ns = ?NS_VCARD, name = 'N', children = [X || X <- NElts, X /= none]},
                    #xmlel{ns = ?NS_VCARD, name = 'ORG', children = [X || X <- OElts, X /= none]},
		    #xmlel{ns = ?NS_VCARD, name = 'ADR', children = [X || X <- AElts, X /= none]}])
     }.

ldap_attribute_to_vcard(vCard, {"fn", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'FN'}, Value);

ldap_attribute_to_vcard(vCard, {"nickname", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'NICKNAME'}, Value);

ldap_attribute_to_vcard(vCard, {"title", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'TITLE'}, Value);

ldap_attribute_to_vcard(vCard, {"bday", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'BDAY'}, Value);

ldap_attribute_to_vcard(vCard, {"url", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'URL'}, Value);

ldap_attribute_to_vcard(vCard, {"desc", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'DESC'}, Value);

ldap_attribute_to_vcard(vCard, {"role", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'ROLE'}, Value);

ldap_attribute_to_vcard(vCard, {"tel", Value}) ->
    #xmlel{ns = ?NS_VCARD, name = 'TEL', children = [
        #xmlel{ns = ?NS_VCARD, name = 'VOICE'},
        #xmlel{ns = ?NS_VCARD, name = 'WORK'},
        exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'NUMBER'}, Value)]};

ldap_attribute_to_vcard(vCard, {"email", Value}) ->
    #xmlel{ns = ?NS_VCARD, name = 'EMAIL', children = [
        #xmlel{ns = ?NS_VCARD, name = 'INTERNET'},
        #xmlel{ns = ?NS_VCARD, name = 'PREF'},
        exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'USERID'}, Value)]};

ldap_attribute_to_vcard(vCard, {"photo", Value}) ->
    #xmlel{ns = ?NS_VCARD, name = 'PHOTO', children = [
        exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'BINVAL'},
          jlib:encode_base64(Value))]};

ldap_attribute_to_vcard(vCardN, {"family", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'FAMILY'}, Value);

ldap_attribute_to_vcard(vCardN, {"given", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'GIVEN'}, Value);

ldap_attribute_to_vcard(vCardN, {"middle", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'MIDDLE'}, Value);

ldap_attribute_to_vcard(vCardO, {"orgname", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'ORGNAME'}, Value);

ldap_attribute_to_vcard(vCardO, {"orgunit", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'ORGUNIT'}, Value);

ldap_attribute_to_vcard(vCardA, {"locality", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'LOCALITY'}, Value);

ldap_attribute_to_vcard(vCardA, {"street", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'STREET'}, Value);

ldap_attribute_to_vcard(vCardA, {"ctry", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'CTRY'}, Value);

ldap_attribute_to_vcard(vCardA, {"region", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'REGION'}, Value);

ldap_attribute_to_vcard(vCardA, {"pcode", Value}) ->
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'PCODE'}, Value);

ldap_attribute_to_vcard(_, _) ->
    none.

-define(TLFIELD(Type, Label, Var),
	#xmlel{ns = ?NS_VCARD, name = 'field', attrs = [
	    ?XMLATTR(<<"type">>, Type),
	    ?XMLATTR(<<"label">>, translate:translate(Lang, Label)),
	    ?XMLATTR(<<"var">>, Var)]}).

-define(FORM(JID, SearchFields),
	[#xmlel{ns = ?NS_SEARCH, name = 'instructions', children =
	  [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "You need an x:data capable client to search"))}]},
	 #xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
           [?XMLATTR(<<"type">>, <<"form">>)], children =
	  [#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	    [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "Search users in ") ++
	      exmpp_jid:to_list(JID))}]},
	   #xmlel{ns = ?NS_SEARCH, name = 'instructions', children =
	    [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "Fill in fields to search "
					    "for any matching Jabber User"))}]}
	  ] ++ lists:map(fun({X,Y}) -> ?TLFIELD(<<"text-single">>, X, list_to_binary(Y)) end, SearchFields)}]).
=======
    case eldap_filter:parse(RFC2254_Filter,
			    [{<<"%u">>, User}])
	of
      {ok, EldapFilter} ->
	  case eldap_pool:search(Eldap_ID,
				 [{base, Base}, {filter, EldapFilter},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, VCardAttrs}])
	      of
	    #eldap_search_result{entries = [E | _]} -> E;
	    _ -> false
	  end;
      _ -> false
    end.

ldap_attributes_to_vcard(Attributes, VCardMap, UD) ->
    Attrs = lists:map(fun ({VCardName, _, _}) ->
			      {stringprep:tolower(VCardName),
			       map_vcard_attr(VCardName, Attributes, VCardMap,
					      UD)}
		      end,
		      VCardMap),
    Elts = [ldap_attribute_to_vcard(vCard, Attr)
	    || Attr <- Attrs],
    NElts = [ldap_attribute_to_vcard(vCardN, Attr)
	     || Attr <- Attrs],
    OElts = [ldap_attribute_to_vcard(vCardO, Attr)
	     || Attr <- Attrs],
    AElts = [ldap_attribute_to_vcard(vCardA, Attr)
	     || Attr <- Attrs],
    [#xmlel{name = <<"vCard">>,
	    attrs = [{<<"xmlns">>, ?NS_VCARD}],
	    children =
		lists:append([X || X <- Elts, X /= none],
			     [#xmlel{name = <<"N">>, attrs = [],
				     children = [X || X <- NElts, X /= none]},
			      #xmlel{name = <<"ORG">>, attrs = [],
				     children = [X || X <- OElts, X /= none]},
			      #xmlel{name = <<"ADR">>, attrs = [],
				     children =
					 [X || X <- AElts, X /= none]}])}].

ldap_attribute_to_vcard(vCard, {<<"fn">>, Value}) ->
    #xmlel{name = <<"FN">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard,
			{<<"nickname">>, Value}) ->
    #xmlel{name = <<"NICKNAME">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"title">>, Value}) ->
    #xmlel{name = <<"TITLE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"bday">>, Value}) ->
    #xmlel{name = <<"BDAY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"url">>, Value}) ->
    #xmlel{name = <<"URL">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"desc">>, Value}) ->
    #xmlel{name = <<"DESC">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"role">>, Value}) ->
    #xmlel{name = <<"ROLE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"tel">>, Value}) ->
    #xmlel{name = <<"TEL">>, attrs = [],
	   children =
	       [#xmlel{name = <<"VOICE">>, attrs = [], children = []},
		#xmlel{name = <<"WORK">>, attrs = [], children = []},
		#xmlel{name = <<"NUMBER">>, attrs = [],
		       children = [{xmlcdata, Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"email">>, Value}) ->
    #xmlel{name = <<"EMAIL">>, attrs = [],
	   children =
	       [#xmlel{name = <<"INTERNET">>, attrs = [],
		       children = []},
		#xmlel{name = <<"PREF">>, attrs = [], children = []},
		#xmlel{name = <<"USERID">>, attrs = [],
		       children = [{xmlcdata, Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"photo">>, Value}) ->
    #xmlel{name = <<"PHOTO">>, attrs = [],
	   children =
	       [#xmlel{name = <<"TYPE">>, attrs = [],
		       children = [{xmlcdata, <<"image/jpeg">>}]},
		#xmlel{name = <<"BINVAL">>, attrs = [],
		       children = [{xmlcdata, jlib:encode_base64(Value)}]}]};
ldap_attribute_to_vcard(vCardN,
			{<<"family">>, Value}) ->
    #xmlel{name = <<"FAMILY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardN, {<<"given">>, Value}) ->
    #xmlel{name = <<"GIVEN">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardN,
			{<<"middle">>, Value}) ->
    #xmlel{name = <<"MIDDLE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardO,
			{<<"orgname">>, Value}) ->
    #xmlel{name = <<"ORGNAME">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardO,
			{<<"orgunit">>, Value}) ->
    #xmlel{name = <<"ORGUNIT">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
			{<<"locality">>, Value}) ->
    #xmlel{name = <<"LOCALITY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
			{<<"street">>, Value}) ->
    #xmlel{name = <<"STREET">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA, {<<"ctry">>, Value}) ->
    #xmlel{name = <<"CTRY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
			{<<"region">>, Value}) ->
    #xmlel{name = <<"REGION">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA, {<<"pcode">>, Value}) ->
    #xmlel{name = <<"PCODE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(_, _) -> none.

-define(TLFIELD(Type, Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

-define(FORM(JID, SearchFields),
	[#xmlel{name = <<"instructions">>, attrs = [],
		children =
		    [{xmlcdata,
		      translate:translate(Lang,
					  <<"You need an x:data capable client to "
					    "search">>)}]},
	 #xmlel{name = <<"x">>,
		attrs =
		    [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
		children =
		    [#xmlel{name = <<"title">>, attrs = [],
			    children =
				[{xmlcdata,
				  <<(translate:translate(Lang,
							 <<"Search users in ">>))/binary,
				    (jlib:jid_to_string(JID))/binary>>}]},
		     #xmlel{name = <<"instructions">>, attrs = [],
			    children =
				[{xmlcdata,
				  translate:translate(Lang,
						      <<"Fill in fields to search for any matching "
							"Jabber User">>)}]}]
		      ++
		      lists:map(fun ({X, Y}) ->
					?TLFIELD(<<"text-single">>, X, Y)
				end,
				SearchFields)}]).
>>>>>>> upstream/master

do_route(State, From, To, Packet) ->
    spawn(?MODULE, route, [State, From, To, Packet]).

route(State, From, To, Packet) ->
    User = exmpp_jid:node(To),
    Resource = exmpp_jid:resource(To),
    ServerHost = State#state.serverhost,
<<<<<<< HEAD
    if
	(User /= undefined) or (Resource /= undefined) ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'service-unavailable'),
	    ejabberd_router:route(To, From, Err);
	true ->
	    try
		Request = exmpp_iq:get_request(Packet),
		Type = exmpp_iq:get_type(Packet),
		Lang = exmpp_stanza:get_lang(Packet),
		case {Type, Request#xmlel.ns} of
		    {set, ?NS_SEARCH} ->
                        XDataEl = find_xdata_el(Request),
                        case XDataEl of
                            false ->
				Err = exmpp_iq:error(Packet, 'bad-request'),
                                ejabberd_router:route(To, From, Err);
                            _ ->
                                XData = jlib:parse_xdata_submit(XDataEl),
                                case XData of
                                    invalid ->
					Err = exmpp_iq:error(Packet,
					  'bad-request'),
                                        ejabberd_router:route(To, From,
                                                              Err);
                                    _ ->
					Result = #xmlel{
					  ns = ?NS_SEARCH,
					  name = 'query',
					  children =
                                             [#xmlel{ns = ?NS_DATA_FORMS,
					      name = 'x',
					      attrs = [?XMLATTR(<<"type">>,
						  <<"result">>)],
					      children = search_result(Lang, To, State, XData)}]},
					ResIQ = exmpp_iq:result(Packet,
					  Result),
                                        ejabberd_router:route(
                                          To, From, ResIQ)
                                end
                        end;
		    {get, ?NS_SEARCH} ->
                        SearchFields = State#state.search_fields,
			Result = #xmlel{ns = ?NS_SEARCH, name = 'query',
			  children = ?FORM(To, SearchFields)},
			ResIQ = exmpp_iq:result(Packet, Result),
			ejabberd_router:route(To,
					      From,
					      ResIQ);
		    {set, ?NS_DISCO_INFO} ->
			Err = exmpp_iq:error(Packet, 'not-allowed'),
			ejabberd_router:route(To, From, Err);
		    {get, ?NS_DISCO_INFO} ->
			ServerHostB = list_to_binary(ServerHost),
			Info = ejabberd_hooks:run_fold(
				 disco_info, ServerHostB, [],
				 [ServerHost, ?MODULE, <<>>, ""]),
			Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
			  children = Info ++ [
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'identity',
			      attrs = [
				?XMLATTR(<<"category">>, <<"directory">>),
				?XMLATTR(<<"type">>, <<"user">>),
				?XMLATTR(<<"name">>, translate:translate(Lang,
				    "vCard User Search"))]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR(<<"var">>, ?NS_SEARCH_s)]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR(<<"var">>, ?NS_VCARD_s)]}
			  ]},
			ResIQ = exmpp_iq:result(Packet, Result),
                        ejabberd_router:route(To,
                                              From,
                                              ResIQ);
		    {set, ?NS_DISCO_ITEMS} ->
			Err = exmpp_iq:error(Packet, 'not-allowed'),
			ejabberd_router:route(To, From, Err);
		    {get, ?NS_DISCO_ITEMS} ->
			Result = #xmlel{ns = ?NS_DISCO_ITEMS, name = 'query'},
			ResIQ = exmpp_iq:result(Packet, Result),
			ejabberd_router:route(To,
					      From,
					      ResIQ);
		    {get, ?NS_VCARD} ->
			Result = #xmlel{ns = ?NS_VCARD, name = 'vCard',
			  children = iq_get_vcard(Lang)},
			ResIQ = exmpp_iq:result(Packet, Result),
			ejabberd_router:route(To,
					      From,
					      ResIQ);
		    _ ->
			Err = exmpp_iq:error(Packet, 'service-unavailable'),
			ejabberd_router:route(To, From, Err)
		end
	    catch
		_ ->
		    Err1 = exmpp_iq:error(Packet, 'service-unavailable'),
		    ejabberd_router:route(To, From, Err1)
	    end
    end.

iq_get_vcard(Lang) ->
    [
      #xmlel{ns = ?NS_SEARCH, name = 'FN', children = [
	  #xmlcdata{cdata = <<"ejabberd/mod_vcard">>}]},
      #xmlel{ns = ?NS_SEARCH, name = 'URL', children = [
	  #xmlcdata{cdata = list_to_binary(?EJABBERD_URI)}]},
      #xmlel{ns = ?NS_SEARCH, name ='DESC', children = [
	  #xmlcdata{cdata = list_to_binary(
	      translate:translate(Lang, "ejabberd vCard module") ++
	      "\nCopyright (c) 2002-2012 ProcessOne")}]}
    ].

search_result(Lang, JID, State, Data) ->
    SearchReported = State#state.search_reported,
    Header = [#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	       [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "Search Results for ") ++
		 exmpp_jid:to_list(JID))}]},
	      #xmlel{ns = ?NS_DATA_FORMS, name = 'reported', children =
	       [?TLFIELD(<<"text-single">>, "Jabber ID", <<"jid">>)] ++
	       lists:map(
		 fun({Name, Value}) -> ?TLFIELD(<<"text-single">>, Name, list_to_binary(Value)) end,
		 SearchReported)
	      }],
=======
    if (User /= <<"">>) or (Resource /= <<"">>) ->
	   Err = jlib:make_error_reply(Packet,
				       ?ERR_SERVICE_UNAVAILABLE),
	   ejabberd_router:route(To, From, Err);
       true ->
	   IQ = jlib:iq_query_info(Packet),
	   case IQ of
	     #iq{type = Type, xmlns = ?NS_SEARCH, lang = Lang,
		 sub_el = SubEl} ->
		 case Type of
		   set ->
		       XDataEl = find_xdata_el(SubEl),
		       case XDataEl of
			 false ->
			     Err = jlib:make_error_reply(Packet,
							 ?ERR_BAD_REQUEST),
			     ejabberd_router:route(To, From, Err);
			 _ ->
			     XData = jlib:parse_xdata_submit(XDataEl),
			     case XData of
			       invalid ->
				   Err = jlib:make_error_reply(Packet,
							       ?ERR_BAD_REQUEST),
				   ejabberd_router:route(To, From, Err);
			       _ ->
				   ResIQ = IQ#iq{type = result,
						 sub_el =
						     [#xmlel{name = <<"query">>,
							     attrs =
								 [{<<"xmlns">>,
								   ?NS_SEARCH}],
							     children =
								 [#xmlel{name =
									     <<"x">>,
									 attrs =
									     [{<<"xmlns">>,
									       ?NS_XDATA},
									      {<<"type">>,
									       <<"result">>}],
									 children
									     =
									     search_result(Lang,
											   To,
											   State,
											   XData)}]}]},
				   ejabberd_router:route(To, From,
							 jlib:iq_to_xml(ResIQ))
			     end
		       end;
		   get ->
		       SearchFields = State#state.search_fields,
		       ResIQ = IQ#iq{type = result,
				     sub_el =
					 [#xmlel{name = <<"query">>,
						 attrs =
						     [{<<"xmlns">>,
						       ?NS_SEARCH}],
						 children =
						     ?FORM(To, SearchFields)}]},
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
		 end;
	     #iq{type = Type, xmlns = ?NS_DISCO_INFO, lang = Lang} ->
		 case Type of
		   set ->
		       Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
		       ejabberd_router:route(To, From, Err);
		   get ->
		       Info = ejabberd_hooks:run_fold(disco_info, ServerHost,
						      [],
						      [ServerHost, ?MODULE,
						       <<"">>, <<"">>]),
		       ResIQ = IQ#iq{type = result,
				     sub_el =
					 [#xmlel{name = <<"query">>,
						 attrs =
						     [{<<"xmlns">>,
						       ?NS_DISCO_INFO}],
						 children =
						     [#xmlel{name =
								 <<"identity">>,
							     attrs =
								 [{<<"category">>,
								   <<"directory">>},
								  {<<"type">>,
								   <<"user">>},
								  {<<"name">>,
								   translate:translate(Lang,
										       <<"vCard User Search">>)}],
							     children = []},
						      #xmlel{name =
								 <<"feature">>,
							     attrs =
								 [{<<"var">>,
								   ?NS_SEARCH}],
							     children = []},
						      #xmlel{name =
								 <<"feature">>,
							     attrs =
								 [{<<"var">>,
								   ?NS_VCARD}],
							     children = []}]
						       ++ Info}]},
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
		 end;
	     #iq{type = Type, xmlns = ?NS_DISCO_ITEMS} ->
		 case Type of
		   set ->
		       Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
		       ejabberd_router:route(To, From, Err);
		   get ->
		       ResIQ = IQ#iq{type = result,
				     sub_el =
					 [#xmlel{name = <<"query">>,
						 attrs =
						     [{<<"xmlns">>,
						       ?NS_DISCO_ITEMS}],
						 children = []}]},
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
		 end;
	     #iq{type = get, xmlns = ?NS_VCARD, lang = Lang} ->
		 ResIQ = IQ#iq{type = result,
			       sub_el =
				   [#xmlel{name = <<"vCard">>,
					   attrs = [{<<"xmlns">>, ?NS_VCARD}],
					   children = iq_get_vcard(Lang)}]},
		 ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
	     _ ->
		 Err = jlib:make_error_reply(Packet,
					     ?ERR_SERVICE_UNAVAILABLE),
		 ejabberd_router:route(To, From, Err)
	   end
    end.

iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>, attrs = [],
	    children = [{xmlcdata, <<"ejabberd/mod_vcard">>}]},
     #xmlel{name = <<"URL">>, attrs = [],
	    children = [{xmlcdata, ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>, attrs = [],
	    children =
		[{xmlcdata,
		  <<(translate:translate(Lang,
					 <<"ejabberd vCard module">>))/binary,
		    "\nCopyright (c) 2003-2014 ProcessOne">>}]}].

-define(LFIELD(Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

search_result(Lang, JID, State, Data) ->
    SearchReported = State#state.search_reported,
    Header = [#xmlel{name = <<"title">>, attrs = [],
		     children =
			 [{xmlcdata,
			   <<(translate:translate(Lang,
						  <<"Search Results for ">>))/binary,
			     (jlib:jid_to_string(JID))/binary>>}]},
	      #xmlel{name = <<"reported">>, attrs = [],
		     children =
			 [?TLFIELD(<<"text-single">>, <<"Jabber ID">>,
				   <<"jid">>)]
			   ++
			   lists:map(fun ({Name, Value}) ->
					     ?TLFIELD(<<"text-single">>, Name,
						      Value)
				     end,
				     SearchReported)}],
>>>>>>> upstream/master
    case search(State, Data) of
      error -> Header;
      Result -> Header ++ Result
    end.

-define(FIELD(Var, Val),
<<<<<<< HEAD
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	  [?XMLATTR(<<"var">>, Var)], children =
	  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
	      [#xmlcdata{cdata = Val}]}]}).
=======
	#xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).
>>>>>>> upstream/master

search(State, Data) ->
    Base = State#state.base,
    SearchFilter = State#state.search_filter,
    Eldap_ID = State#state.eldap_id,
    UIDs = State#state.uids,
    Limit = State#state.matches,
    ReportedAttrs = State#state.search_reported_attrs,
<<<<<<< HEAD
    Filter = eldap:'and'([SearchFilter, eldap_utils:make_filter(Data, UIDs)]),
    case eldap_pool:search(Eldap_ID,
                           [{base, Base},
                            {filter, Filter},
                            {limit, Limit},
                            {deref_aliases, State#state.deref_aliases},
                            {attributes, ReportedAttrs}]) of
	#eldap_search_result{entries = E} ->
	    search_items(E, State);
	_ ->
	    error
=======
    Filter = eldap:'and'([SearchFilter,
			  eldap_utils:make_filter(Data, UIDs)]),
    case eldap_pool:search(Eldap_ID,
			   [{base, Base}, {filter, Filter}, {limit, Limit},
			    {deref_aliases, State#state.deref_aliases},
			    {attributes, ReportedAttrs}])
	of
      #eldap_search_result{entries = E} ->
	  search_items(E, State);
      _ -> error
>>>>>>> upstream/master
    end.

search_items(Entries, State) ->
    LServer = State#state.serverhost,
    SearchReported = State#state.search_reported,
    VCardMap = State#state.vcard_map,
<<<<<<< HEAD
	UIDs = State#state.uids,
    Attributes = lists:map(
		   fun(E) ->
			   #eldap_entry{attributes = Attrs} = E,
			   Attrs
		   end, Entries),
    lists:flatmap(
      fun(Attrs) ->
	      case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
	      {U, UIDAttrFormat} ->
	          case eldap_utils:get_user_part(U, UIDAttrFormat) of
		      {ok, Username} ->
		          case ejabberd_auth:is_user_exists(Username, LServer) of
			      true ->
			        RFields = lists:map(
					  fun({_, VCardName}) ->
						  {VCardName,
						   map_vcard_attr(
						     VCardName,
						     Attrs,
						     VCardMap,
						     {Username, ?MYNAME})}
					  end, SearchReported),
			        Result = [?FIELD(<<"jid">>, list_to_binary(Username ++ "@" ++ LServer))] ++
				    [?FIELD(list_to_binary(Name), list_to_binary(Value)) || {Name, Value} <- RFields],
			        [#xmlel{ns = ?NS_DATA_FORMS, name = 'item', children = Result}];
			      _ ->
			          []
		          end;
		      _ ->
		         []
	          end;
          "" ->
		      []
		  end
      end, Attributes).

remove_user(_User) ->
    true.
=======
    UIDs = State#state.uids,
    Attributes = lists:map(fun (E) ->
				   #eldap_entry{attributes = Attrs} = E, Attrs
			   end,
			   Entries),
    lists:flatmap(fun (Attrs) ->
			  case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
			    {U, UIDAttrFormat} ->
				case eldap_utils:get_user_part(U, UIDAttrFormat)
				    of
				  {ok, Username} ->
				      case
					ejabberd_auth:is_user_exists(Username,
								     LServer)
					  of
					true ->
					    RFields = lists:map(fun ({_,
								      VCardName}) ->
									{VCardName,
									 map_vcard_attr(VCardName,
											Attrs,
											VCardMap,
											{Username,
											 ?MYNAME})}
								end,
								SearchReported),
					    Result = [?FIELD(<<"jid">>,
							     <<Username/binary,
							       "@",
							       LServer/binary>>)]
						       ++
						       [?FIELD(Name, Value)
							|| {Name, Value}
							       <- RFields],
					    [#xmlel{name = <<"item">>,
						    attrs = [],
						    children = Result}];
					_ -> []
				      end;
				  _ -> []
				end;
			    <<"">> -> []
			  end
		  end,
		  Attributes).

remove_user(_User) -> true.
>>>>>>> upstream/master

%%%-----------------------
%%% Auxiliary functions.
%%%-----------------------

map_vcard_attr(VCardName, Attributes, Pattern, UD) ->
    Res = lists:filter(fun ({Name, _, _}) ->
			       eldap_utils:case_insensitive_match(Name,
								  VCardName)
		       end,
		       Pattern),
    case Res of
      [{_, Str, Attrs}] ->
	  process_pattern(Str, UD,
			  [eldap_utils:get_ldap_attr(X, Attributes)
			   || X <- Attrs]);
      _ -> <<"">>
    end.

process_pattern(Str, {User, Domain}, AttrValues) ->
    eldap_filter:do_sub(Str,
			[{<<"%u">>, User}, {<<"%d">>, Domain}] ++
			  [{<<"%s">>, V, 1} || V <- AttrValues]).

find_xdata_el(#xmlel{children = SubEls}) ->
<<<<<<< HEAD
%find_xdata_el({xmlelement, _Name, _Attrs, SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([#xmlel{ns = ?NS_DATA_FORMS} = El | _Els]) ->
    El;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

parse_options(Host, Opts) ->
    MyHost = gen_mod:get_opt_host(Host, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    Matches = case gen_mod:get_opt(matches, Opts, 30) of
		infinity  -> 0;
		N         -> N
	    end,
    Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, ?PROCNAME)),
    LDAPServers = case gen_mod:get_opt(ldap_servers, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_servers, Host});
		      S -> S
		  end,
    LDAPBackups = case gen_mod:get_opt(ldap_backups, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_servers, Host});
		      Backups -> Backups
		  end,
    LDAPEncrypt = case gen_mod:get_opt(ldap_encrypt, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_encrypt, Host});
		      E -> E
	          end,
    LDAPTLSVerify = case gen_mod:get_opt(ldap_tls_verify, Opts, undefined) of
			undefined ->
			    ejabberd_config:get_local_option({ldap_tls_verify, Host});
			Verify -> Verify
		    end,
    LDAPTLSCAFile = case gen_mod:get_opt(ldap_tls_cacertfile, Opts, undefined) of
                        undefined ->
                            ejabberd_config:get_local_option({ldap_tls_cacertfile, Host});
                        CAFile -> CAFile
                    end,
    LDAPTLSDepth = case gen_mod:get_opt(ldap_tls_depth, Opts, undefined) of
                       undefined ->
                           ejabberd_config:get_local_option({ldap_tls_depth, Host});
                       Depth ->
                           Depth
                   end,
    LDAPPortTemp = case gen_mod:get_opt(ldap_port, Opts, undefined) of
		       undefined ->
			   ejabberd_config:get_local_option({ldap_port, Host});
		       PT -> PT
	           end,
    LDAPPort = case LDAPPortTemp of
		   undefined ->
		       case LDAPEncrypt of
			   tls -> ?LDAPS_PORT;
			   starttls -> ?LDAP_PORT;
			   _ -> ?LDAP_PORT
		       end;
		   P -> P
	       end,
    LDAPBase = case gen_mod:get_opt(ldap_base, Opts, undefined) of
		   undefined ->
		       ejabberd_config:get_local_option({ldap_base, Host});
		   B -> B
	       end,
	UIDs = case gen_mod:get_opt(ldap_uids, Opts, undefined) of
	    undefined ->
		    case ejabberd_config:get_local_option({ldap_uids, Host}) of
			undefined -> [{"uid", "%u"}];
			UI -> eldap_utils:uids_domain_subst(Host, UI)
			end;
		UI -> eldap_utils:uids_domain_subst(Host, UI)
		end,
    RootDN = case gen_mod:get_opt(ldap_rootdn, Opts, undefined) of
		 undefined ->
		     case ejabberd_config:get_local_option({ldap_rootdn, Host}) of
			 undefined -> "";
			 RDN -> RDN
		     end;
		 RDN -> RDN
	     end,
    Password = case gen_mod:get_opt(ldap_password, Opts, undefined) of
		   undefined ->
		       case ejabberd_config:get_local_option({ldap_password, Host}) of
			   undefined -> "";
			   Pass -> Pass
		       end;
		   Pass -> Pass
	       end,
    SubFilter = lists:flatten(eldap_utils:generate_subfilter(UIDs)),
    UserFilter = case gen_mod:get_opt(ldap_filter, Opts, undefined) of
		     undefined ->
			 case ejabberd_config:get_local_option({ldap_filter, Host}) of
			     undefined -> SubFilter;
			     "" -> SubFilter;
			     F ->
                                 eldap_utils:check_filter(F),
                                 "(&" ++ SubFilter ++ F ++ ")"
			 end;
		     "" -> SubFilter;
		     F ->
                         eldap_utils:check_filter(F),
                         "(&" ++ SubFilter ++ F ++ ")"
		 end,
    {ok, SearchFilter} = eldap_filter:parse(
			   eldap_filter:do_sub(UserFilter, [{"%u","*"}])),
    VCardMap = gen_mod:get_opt(ldap_vcard_map, Opts, ?VCARD_MAP),
    SearchFields = gen_mod:get_opt(ldap_search_fields, Opts, ?SEARCH_FIELDS),
    SearchReported = gen_mod:get_opt(ldap_search_reported, Opts, ?SEARCH_REPORTED),
    %% In search requests we need to fetch only attributes defined
    %% in vcard-map and search-reported. In some cases,
    %% this will essentially reduce network traffic from an LDAP server.
	UIDAttrs = [UAttr || {UAttr, _} <- UIDs],
    VCardMapAttrs = lists:usort(
		      lists:append([A || {_, _, A} <- VCardMap]) ++ UIDAttrs),
    SearchReportedAttrs =
	lists:usort(lists:flatmap(
		      fun({_, N}) ->
			      case lists:keysearch(N, 1, VCardMap) of
				  {value, {_, _, L}} -> L;
				  _ -> []
			      end
		      end, SearchReported) ++ UIDAttrs),
    DerefAliases = case gen_mod:get_opt(deref_aliases, Opts, undefined) of
                       undefined ->
                           case ejabberd_config:get_local_option(
                                  {deref_aliases, Host}) of
                               undefined -> never;
                               D -> D
                           end;
                       D -> D
                   end,
    #state{serverhost = Host,
	   myhost = MyHost,
	   eldap_id = Eldap_ID,
	   search = Search,
	   servers = LDAPServers,
	   backups = LDAPBackups,
	   port = LDAPPort,
	   tls_options = [{encrypt, LDAPEncrypt},
			  {tls_verify, LDAPTLSVerify},
                          {tls_cacertfile, LDAPTLSCAFile},
                          {tls_depth, LDAPTLSDepth}],
	   dn = RootDN,
	   base = LDAPBase,
	   password = Password,
	   uids = UIDs,
	   vcard_map = VCardMap,
=======
    find_xdata_el1(SubEls).

find_xdata_el1([]) -> false;
find_xdata_el1([#xmlel{name = Name, attrs = Attrs,
		       children = SubEls}
		| Els]) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
      ?NS_XDATA ->
	  #xmlel{name = Name, attrs = Attrs, children = SubEls};
      _ -> find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) -> find_xdata_el1(Els).

parse_options(Host, Opts) ->
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"vjud.@HOST@">>),
    Search = gen_mod:get_opt(search, Opts,
                             fun(B) when is_boolean(B) -> B end,
                             true),
    Matches = gen_mod:get_opt(matches, Opts,
                              fun(infinity) -> 0;
                                 (I) when is_integer(I), I>0 -> I
                              end, 30),
    Eldap_ID = jlib:atom_to_binary(gen_mod:get_module_proc(Host, ?PROCNAME)),
    Cfg = eldap_utils:get_config(Host, Opts),
    UIDsTemp = eldap_utils:get_opt(
                 {ldap_uids, Host}, Opts,
                 fun(Us) ->
                         lists:map(
                           fun({U, P}) ->
                                   {iolist_to_binary(U),
                                    iolist_to_binary(P)};
                              ({U}) ->
                                   {iolist_to_binary(U)}
                           end, Us)
                 end, [{<<"uid">>, <<"%u">>}]),
    UIDs = eldap_utils:uids_domain_subst(Host, UIDsTemp),
    SubFilter = eldap_utils:generate_subfilter(UIDs),
    UserFilter = case eldap_utils:get_opt(
                        {ldap_filter, Host}, Opts,
                        fun check_filter/1, <<"">>) of
                     <<"">> ->
			 SubFilter;
                     F ->
                         <<"(&", SubFilter/binary, F/binary, ")">>
                 end,
    {ok, SearchFilter} =
	eldap_filter:parse(eldap_filter:do_sub(UserFilter,
					       [{<<"%u">>, <<"*">>}])),
    VCardMap = gen_mod:get_opt(ldap_vcard_map, Opts,
                               fun(Ls) ->
                                       lists:map(
                                         fun({S, [{P, L}]}) ->
                                                 {iolist_to_binary(S),
                                                  iolist_to_binary(P),
                                                  [iolist_to_binary(E)
                                                   || E <- L]}
                                         end, Ls)
                               end, ?VCARD_MAP),
    SearchFields = gen_mod:get_opt(ldap_search_fields, Opts,
                                   fun(Ls) ->
                                           [{iolist_to_binary(S),
                                             iolist_to_binary(P)}
                                            || {S, P} <- Ls]
                                   end, ?SEARCH_FIELDS),
    SearchReported = gen_mod:get_opt(ldap_search_reported, Opts,
                                     fun(Ls) ->
                                             [{iolist_to_binary(S),
                                               iolist_to_binary(P)}
                                              || {S, P} <- Ls]
                                     end, ?SEARCH_REPORTED),
    UIDAttrs = [UAttr || {UAttr, _} <- UIDs],
    VCardMapAttrs = lists:usort(lists:append([A
					      || {_, _, A} <- VCardMap])
				  ++ UIDAttrs),
    SearchReportedAttrs = lists:usort(lists:flatmap(fun ({_,
							  N}) ->
							    case
							      lists:keysearch(N,
									      1,
									      VCardMap)
								of
							      {value,
							       {_, _, L}} ->
								  L;
							      _ -> []
							    end
						    end,
						    SearchReported)
					++ UIDAttrs),
    #state{serverhost = Host, myhost = MyHost,
	   eldap_id = Eldap_ID, search = Search,
	   servers = Cfg#eldap_config.servers,
	   backups = Cfg#eldap_config.backups,
           port = Cfg#eldap_config.port,
	   tls_options = Cfg#eldap_config.tls_options,
	   dn = Cfg#eldap_config.dn,
           password = Cfg#eldap_config.password,
           base = Cfg#eldap_config.base,
           deref_aliases = Cfg#eldap_config.deref_aliases,
	   uids = UIDs, vcard_map = VCardMap,
>>>>>>> upstream/master
	   vcard_map_attrs = VCardMapAttrs,
	   user_filter = UserFilter, search_filter = SearchFilter,
	   search_fields = SearchFields,
	   search_reported = SearchReported,
	   search_reported_attrs = SearchReportedAttrs,
<<<<<<< HEAD
           deref_aliases = DerefAliases,
	   matches = Matches
	  }.
=======
	   matches = Matches}.

transform_module_options(Opts) ->
    lists:map(
      fun({ldap_vcard_map, Map}) ->
              NewMap = lists:map(
                         fun({Field, Pattern, Attrs}) ->
                                 {Field, [{Pattern, Attrs}]};
                            (Opt) ->
                                 Opt
                         end, Map),
              {ldap_vcard_map, NewMap};
         (Opt) ->
              Opt
      end, Opts).

check_filter(F) ->
    NewF = iolist_to_binary(F),
    {ok, _} = eldap_filter:parse(NewF),
    NewF.
>>>>>>> upstream/master
