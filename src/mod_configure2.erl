%%%----------------------------------------------------------------------
%%% File    : mod_configure2.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 26 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_configure2).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD

-define(NS_ECONFIGURE, 'http://ejabberd.jabberstudio.org/protocol/configure').
-define(NS_ECONFIGURE_s, "http://ejabberd.jabberstudio.org/protocol/configure").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, list_to_binary(Host), ?NS_ECONFIGURE,
				  ?MODULE, process_local_iq, IQDisc),
    % Add nss/names/attrs used by this module to the known lists of Exmpp.
    exmpp_xml:add_known_nss(xmpp, [?NS_ECONFIGURE]),
    exmpp_xml:add_known_elems(xmpp, [
	'access',
	'acls',
	'body',
	'info',
	'jid',
	'last',
	'registration-watchers',
	'subject',
	'welcome-message'
      ]),
    ok.

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, list_to_binary(Host), ?NS_ECONFIGURE).


process_local_iq(From, To, #iq{type = Type, payload = Request} = IQ_Rec) ->
    case acl:match_rule(exmpp_jid:prep_domain_as_list(To), configure, From) of
	deny ->
	    exmpp_iq:error(IQ_Rec, 'not-allowed');
	allow ->
	    case Type of
		set ->
		    exmpp_iq:error(IQ_Rec, 'feature-not-implemented');
		    %%case xml:get_tag_attr_s("type", SubEl) of
		    %%    "cancel" ->
		    %%        IQ#iq{type = result,
		    %%		   sub_el = [{xmlelement, "query",
		    %%			      [{"xmlns", XMLNS}], []}]};
		    %%    "submit" ->
		    %%        XData = jlib:parse_xdata_submit(SubEl),
		    %%        case XData of
		    %%    	invalid ->
		    %%    	    IQ#iq{type = error,
		    %%			  sub_el = [SubEl, ?ERR_BAD_REQUEST]};
		    %%    	_ ->
		    %%    	    Node =
		    %%    		string:tokens(
		    %%    		  xml:get_tag_attr_s("node", SubEl),
		    %%    		  "/"),
		    %%    	    case set_form(Node, Lang, XData) of
		    %%    		{result, Res} ->
		    %%    		    IQ#iq{type = result,
		    %%				  sub_el = [{xmlelement, "query",
		    %%					     [{"xmlns", XMLNS}],
		    %%					     Res
		    %%					    }]};
		    %%    		{error, Error} ->
		    %%    		    IQ#iq{type = error,
		    %%				  sub_el = [SubEl, Error]}
		    %%    	    end
		    %%        end;
		    %%    _ ->
		    %%        IQ#iq{type = error,
		    %%		   sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		    %%end;
		get ->
		    case process_get(Request) of
			{result, Res} ->
			    exmpp_iq:result(IQ_Rec, Res);
			{error, Error} ->
			    exmpp_iq:error(IQ_Rec, Error)
		    end
	    end
    end.


process_get(#xmlel{ns = ?NS_ECONFIGURE, name = 'info'}) ->
    S2SConns = ejabberd_s2s:dirty_get_connections(),
    TConns = lists:usort([element(2, C) || C <- S2SConns]),
    Attrs = [?XMLATTR(<<"registered-users">>, mnesia:table_info(passwd, size)),
	     ?XMLATTR(<<"online-users">>, mnesia:table_info(presence, size)),
	     ?XMLATTR(<<"running-nodes">>,
	      length(mnesia:system_info(running_db_nodes))),
	     ?XMLATTR(<<"stopped-nodes">>,
              length(lists:usort(mnesia:system_info(db_nodes) ++
                                 mnesia:system_info(extra_db_nodes)) --
                     mnesia:system_info(running_db_nodes))),
	     ?XMLATTR(<<"outgoing-s2s-servers">>,
	      length(TConns))],
    {result, #xmlel{ns = ?NS_ECONFIGURE, name = 'info', attrs = Attrs}};
process_get(#xmlel{ns = ?NS_ECONFIGURE, name = 'welcome-message', attrs = Attrs}) ->
    {Subj, Body} = case ejabberd_config:get_local_option(welcome_message) of
		       {_Subj, _Body} = SB -> SB;
		       _ -> {"", ""}
		   end,
    {result, #xmlel{ns = ?NS_ECONFIGURE, name = 'welcome-message',
	      attrs = Attrs, children =
	      [#xmlel{ns = ?NS_ECONFIGURE, name = 'subject', children = [#xmlcdata{cdata = list_to_binary(Subj)}]},
	       #xmlel{ns = ?NS_ECONFIGURE, name = 'body', children = [#xmlcdata{cdata = list_to_binary(Body)}]}]}};
process_get(#xmlel{ns = ?NS_ECONFIGURE, name = 'registration-watchers', attrs = Attrs}) ->
    SubEls =
	case ejabberd_config:get_local_option(registration_watchers) of
	    JIDs when is_list(JIDs) ->
		lists:map(fun(JID) ->
				  #xmlel{ns = ?NS_ECONFIGURE, name = 'jid', children = [#xmlcdata{cdata = list_to_binary(JID)}]}
			  end, JIDs);
	    _ ->
		[]
	end,
    {result, #xmlel{ns = ?NS_ECONFIGURE, name = 'registration_watchers', attrs = Attrs, children = SubEls}};
process_get(#xmlel{ns = ?NS_ECONFIGURE, name = 'acls', attrs = Attrs}) ->
    Str = lists:flatten(io_lib:format("~p.", [ets:tab2list(acl)])),
    {result, #xmlel{ns = ?NS_ECONFIGURE, name = 'acls', attrs = Attrs, children = [#xmlcdata{cdata = list_to_binary(Str)}]}};
process_get(#xmlel{ns = ?NS_ECONFIGURE, name = 'access', attrs = Attrs}) ->
    Str =
	lists:flatten(
	  io_lib:format(
	    "~p.",
	    [ets:select(config,
			[{{config, {access, '$1'}, '$2'},
			  [],
			  [{{access, '$1', '$2'}}]}])
	    ])),
    {result, #xmlel{ns = ?NS_ECONFIGURE, name = 'access', attrs = Attrs, children = [#xmlcdata{cdata = list_to_binary(Str)}]}};
process_get(#xmlel{ns = ?NS_ECONFIGURE, name = 'last', attrs = Attrs}) ->
    case catch mnesia:dirty_select(
		 last_activity, [{{last_activity, '_', '$1', '_'}, [], ['$1']}]) of
	{'EXIT', _Reason} ->
	    {error, 'internal-server-error'};
	Vals ->
	    {MegaSecs, Secs, _MicroSecs} = now(),
	    TimeStamp = MegaSecs * 1000000 + Secs,
	    Str = lists:flatten(
		    lists:append(
		      [[integer_to_list(TimeStamp - V), " "] || V <- Vals])),
	    {result, #xmlel{ns = ?NS_ECONFIGURE, name = 'last', attrs = Attrs, children = [#xmlcdata{cdata = list_to_binary(Str)}]}}
    end;
%%process_get({xmlelement, Name, Attrs, SubEls}) ->
%%    {result, };
process_get(_) ->
    {error, 'bad-request'}.

=======
-include("logger.hrl").

-include("jlib.hrl").

-define(NS_ECONFIGURE,
	<<"http://ejabberd.jabberstudio.org/protocol/con"
	  "figure">>).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_ECONFIGURE, ?MODULE, process_local_iq,
				  IQDisc),
    ok.

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_ECONFIGURE).

process_local_iq(From, To,
		 #iq{type = Type, lang = _Lang, sub_el = SubEl} = IQ) ->
    case acl:match_rule(To#jid.lserver, configure, From) of
      deny ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      allow ->
	  case Type of
	    set ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]};
	    %%case xml:get_tag_attr_s("type", SubEl) of
	    %%    "cancel" ->
	    %%        IQ#iq{type = result,
	    %%		   sub_el = [{xmlelement, "query",
	    %%			      [{"xmlns", XMLNS}], []}]};
	    %%    "submit" ->
	    %%        XData = jlib:parse_xdata_submit(SubEl),
	    %%        case XData of
	    %%    	invalid ->
	    %%    	    IQ#iq{type = error,
	    %%			  sub_el = [SubEl, ?ERR_BAD_REQUEST]};
	    %%    	_ ->
	    %%    	    Node =
	    %%    		string:tokens(
	    %%    		  xml:get_tag_attr_s("node", SubEl),
	    %%    		  "/"),
	    %%    	    case set_form(Node, Lang, XData) of
	    %%    		{result, Res} ->
	    %%    		    IQ#iq{type = result,
	    %%				  sub_el = [{xmlelement, "query",
	    %%					     [{"xmlns", XMLNS}],
	    %%					     Res
	    %%					    }]};
	    %%    		{error, Error} ->
	    %%    		    IQ#iq{type = error,
	    %%				  sub_el = [SubEl, Error]}
	    %%    	    end
	    %%        end;
	    %%    _ ->
	    %%        IQ#iq{type = error,
	    %%		   sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
	    %%end;
	    get ->
		case process_get(SubEl) of
		  {result, Res} -> IQ#iq{type = result, sub_el = [Res]};
		  {error, Error} ->
		      IQ#iq{type = error, sub_el = [SubEl, Error]}
		end
	  end
    end.

process_get(#xmlel{name = <<"info">>}) ->
    S2SConns = ejabberd_s2s:dirty_get_connections(),
    TConns = lists:usort([element(2, C) || C <- S2SConns]),
    Attrs = [{<<"registered-users">>,
	      iolist_to_binary(integer_to_list(mnesia:table_info(passwd,
								 size)))},
	     {<<"online-users">>,
	      iolist_to_binary(integer_to_list(mnesia:table_info(presence,
								 size)))},
	     {<<"running-nodes">>,
	      iolist_to_binary(integer_to_list(length(mnesia:system_info(running_db_nodes))))},
	     {<<"stopped-nodes">>,
	      iolist_to_binary(integer_to_list(length(lists:usort(mnesia:system_info(db_nodes)
								     ++
								     mnesia:system_info(extra_db_nodes))
							 --
							 mnesia:system_info(running_db_nodes))))},
	     {<<"outgoing-s2s-servers">>,
	      iolist_to_binary(integer_to_list(length(TConns)))}],
    {result,
     #xmlel{name = <<"info">>,
	    attrs = [{<<"xmlns">>, ?NS_ECONFIGURE} | Attrs],
	    children = []}};
process_get(#xmlel{name = <<"welcome-message">>,
		   attrs = Attrs}) ->
    {Subj, Body} = ejabberd_config:get_option(
                     welcome_message,
                     fun({Subj, Body}) ->
                             {iolist_to_binary(Subj),
                              iolist_to_binary(Body)}
                     end,
                     {<<"">>, <<"">>}),
    {result,
     #xmlel{name = <<"welcome-message">>, attrs = Attrs,
	    children =
		[#xmlel{name = <<"subject">>, attrs = [],
			children = [{xmlcdata, Subj}]},
		 #xmlel{name = <<"body">>, attrs = [],
			children = [{xmlcdata, Body}]}]}};
process_get(#xmlel{name = <<"registration-watchers">>,
		   attrs = Attrs}) ->
    SubEls = ejabberd_config:get_option(
               registration_watchers,
               fun(JIDs) when is_list(JIDs) ->
                       lists:map(
                         fun(J) ->
                                 #xmlel{name = <<"jid">>, attrs = [],
                                        children = [{xmlcdata,
                                                     iolist_to_binary(J)}]}
                         end, JIDs)
               end, []),
    {result,
     #xmlel{name = <<"registration_watchers">>,
	    attrs = Attrs, children = SubEls}};
process_get(#xmlel{name = <<"acls">>, attrs = Attrs}) ->
    Str = iolist_to_binary(io_lib:format("~p.",
					 [ets:tab2list(acl)])),
    {result,
     #xmlel{name = <<"acls">>, attrs = Attrs,
	    children = [{xmlcdata, Str}]}};
process_get(#xmlel{name = <<"access">>,
		   attrs = Attrs}) ->
    Str = iolist_to_binary(io_lib:format("~p.",
					 [ets:select(local_config,
						     [{{local_config, {access, '$1'},
							'$2'},
						       [],
						       [{{access, '$1',
							  '$2'}}]}])])),
    {result,
     #xmlel{name = <<"access">>, attrs = Attrs,
	    children = [{xmlcdata, Str}]}};
process_get(#xmlel{name = <<"last">>, attrs = Attrs}) ->
    case catch mnesia:dirty_select(last_activity,
				   [{{last_activity, '_', '$1', '_'}, [],
				     ['$1']}])
	of
      {'EXIT', _Reason} ->
	  {error, ?ERR_INTERNAL_SERVER_ERROR};
      Vals ->
	  {MegaSecs, Secs, _MicroSecs} = now(),
	  TimeStamp = MegaSecs * 1000000 + Secs,
	  Str = list_to_binary(
                  [[jlib:integer_to_binary(TimeStamp - V),
                    <<" ">>] || V <- Vals]),
	  {result,
	   #xmlel{name = <<"last">>, attrs = Attrs,
		  children = [{xmlcdata, Str}]}}
    end;
%%process_get({xmlelement, Name, Attrs, SubEls}) ->
%%    {result, };
process_get(_) -> {error, ?ERR_BAD_REQUEST}.
>>>>>>> upstream/master
