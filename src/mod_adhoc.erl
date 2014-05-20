%%%----------------------------------------------------------------------
%%% File    : mod_adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Handle incoming ad-doc requests (XEP-0050)
%%% Created : 15 Nov 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
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

-module(mod_adhoc).

-author('henoch@dtek.chalmers.se').

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3,
	 process_sm_iq/3, get_local_commands/5,
	 get_local_identity/5, get_local_features/5,
	 get_sm_commands/5, get_sm_identity/5, get_sm_features/5,
	 ping_item/4, ping_command/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD
-include("adhoc.hrl").

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),

    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_ADHOC,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_ADHOC,
				  ?MODULE, process_sm_iq, IQDisc),
    
    ejabberd_hooks:add(disco_local_identity, HostB, ?MODULE, get_local_identity, 99),
    ejabberd_hooks:add(disco_local_features, HostB, ?MODULE, get_local_features, 99),
    ejabberd_hooks:add(disco_local_items, HostB, ?MODULE, get_local_commands, 99),
    ejabberd_hooks:add(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 99),
    ejabberd_hooks:add(disco_sm_features, HostB, ?MODULE, get_sm_features, 99),
    ejabberd_hooks:add(disco_sm_items, HostB, ?MODULE, get_sm_commands, 99),
    ejabberd_hooks:add(adhoc_local_items, HostB, ?MODULE, ping_item, 100),
    ejabberd_hooks:add(adhoc_local_commands, HostB, ?MODULE, ping_command, 100).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(adhoc_local_commands, HostB, ?MODULE, ping_command, 100),
    ejabberd_hooks:delete(adhoc_local_items, HostB, ?MODULE, ping_item, 100),
    ejabberd_hooks:delete(disco_sm_items, HostB, ?MODULE, get_sm_commands, 99),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 99),
    ejabberd_hooks:delete(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 99),
    ejabberd_hooks:delete(disco_local_items, HostB, ?MODULE, get_local_commands, 99),
    ejabberd_hooks:delete(disco_local_features, HostB, ?MODULE, get_local_features, 99),
    ejabberd_hooks:delete(disco_local_identity, HostB, ?MODULE, get_local_identity, 99),

    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_ADHOC),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_ADHOC).

%-------------------------------------------------------------------------

get_local_commands(Acc, _From, To, <<>>, Lang) ->
    Server = exmpp_jid:domain(To),
    LServer = exmpp_jid:prep_domain_as_list(To),
    Display = gen_mod:get_module_opt(LServer, ?MODULE, report_commands_node, false),
    case Display of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    Nodes = [#xmlel{ns = ?NS_DISCO_ITEMS,
		      name = 'item', attrs =
		      [?XMLATTR(<<"jid">>, Server),
		       ?XMLATTR(<<"node">>, ?NS_ADHOC_s),
                       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]
		      }],
	    {result, Items ++ Nodes}
    end;

get_local_commands(_Acc, From, To, ?NS_ADHOC_b, Lang) ->
    ejabberd_hooks:run_fold(adhoc_local_items, exmpp_jid:prep_domain(To), {result, []}, [From, To, Lang]);

get_local_commands(_Acc, _From, _To, <<"ping">>, _Lang) ->
=======
-include("logger.hrl").

-include("jlib.hrl").

-include("adhoc.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_COMMANDS, ?MODULE, process_local_iq,
				  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_COMMANDS, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
		       get_local_identity, 99),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       get_local_features, 99),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE,
		       get_local_commands, 99),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE,
		       get_sm_identity, 99),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 99),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE,
		       get_sm_commands, 99),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE,
		       ping_item, 100),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE,
		       ping_command, 100).

stop(Host) ->
    ejabberd_hooks:delete(adhoc_local_commands, Host,
			  ?MODULE, ping_command, 100),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE,
			  ping_item, 100),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE,
			  get_sm_commands, 99),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 99),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE,
			  get_sm_identity, 99),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE,
			  get_local_commands, 99),
    ejabberd_hooks:delete(disco_local_features, Host,
			  ?MODULE, get_local_features, 99),
    ejabberd_hooks:delete(disco_local_identity, Host,
			  ?MODULE, get_local_identity, 99),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_COMMANDS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_COMMANDS).

%-------------------------------------------------------------------------

get_local_commands(Acc, _From,
		   #jid{server = Server, lserver = LServer} = _To, <<"">>,
		   Lang) ->
    Display = gen_mod:get_module_opt(LServer, ?MODULE,
				     report_commands_node,
                                     fun(B) when is_boolean(B) -> B end,
                                     false),
    case Display of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, I} -> I;
		    _ -> []
		  end,
	  Nodes = [#xmlel{name = <<"item">>,
			  attrs =
			      [{<<"jid">>, Server}, {<<"node">>, ?NS_COMMANDS},
			       {<<"name">>,
				translate:translate(Lang, <<"Commands">>)}],
			  children = []}],
	  {result, Items ++ Nodes}
    end;
get_local_commands(_Acc, From,
		   #jid{lserver = LServer} = To, ?NS_COMMANDS, Lang) ->
    ejabberd_hooks:run_fold(adhoc_local_items, LServer,
			    {result, []}, [From, To, Lang]);
get_local_commands(_Acc, _From, _To, <<"ping">>,
		   _Lang) ->
>>>>>>> upstream/master
    {result, []};
get_local_commands(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

<<<<<<< HEAD
get_sm_commands(Acc, _From, To, <<>>, Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    Display = gen_mod:get_module_opt(LServer, ?MODULE, report_commands_node, false),
    case Display of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    Nodes = [#xmlel{ns = ?NS_DISCO_ITEMS,
		      name = 'item', attrs =
		      [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(To)),
		       ?XMLATTR(<<"node">>, ?NS_ADHOC_s),
		       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]
		      }],
	    {result, Items ++ Nodes}
    end;

get_sm_commands(_Acc, From, To, ?NS_ADHOC_b, Lang) ->
    ejabberd_hooks:run_fold(adhoc_sm_items, exmpp_jid:prep_domain(To), {result, []}, [From, To, Lang]);

get_sm_commands(Acc, _From, _To, _Node, _Lang) ->
    Acc.
=======
get_sm_commands(Acc, _From,
		#jid{lserver = LServer} = To, <<"">>, Lang) ->
    Display = gen_mod:get_module_opt(LServer, ?MODULE,
				     report_commands_node,
                                     fun(B) when is_boolean(B) -> B end,
                                     false),
    case Display of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, I} -> I;
		    _ -> []
		  end,
	  Nodes = [#xmlel{name = <<"item">>,
			  attrs =
			      [{<<"jid">>, jlib:jid_to_string(To)},
			       {<<"node">>, ?NS_COMMANDS},
			       {<<"name">>,
				translate:translate(Lang, <<"Commands">>)}],
			  children = []}],
	  {result, Items ++ Nodes}
    end;
get_sm_commands(_Acc, From,
		#jid{lserver = LServer} = To, ?NS_COMMANDS, Lang) ->
    ejabberd_hooks:run_fold(adhoc_sm_items, LServer,
			    {result, []}, [From, To, Lang]);
get_sm_commands(Acc, _From, _To, _Node, _Lang) -> Acc.
>>>>>>> upstream/master

%-------------------------------------------------------------------------

%% On disco info request to the ad-hoc node, return automation/command-list.
<<<<<<< HEAD
get_local_identity(Acc, _From, _To, ?NS_ADHOC_b, Lang) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
      [?XMLATTR(<<"category">>, <<"automation">>),
       ?XMLATTR(<<"type">>, <<"command-list">>),
       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]} | Acc];

get_local_identity(Acc, _From, _To, <<"ping">>, Lang) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
      [?XMLATTR(<<"category">>, <<"automation">>),
       ?XMLATTR(<<"type">>, <<"command-node">>),
       ?XMLATTR(<<"name">>, translate:translate(Lang, "Ping"))]} | Acc];

=======
get_local_identity(Acc, _From, _To, ?NS_COMMANDS,
		   Lang) ->
    [#xmlel{name = <<"identity">>,
	    attrs =
		[{<<"category">>, <<"automation">>},
		 {<<"type">>, <<"command-list">>},
		 {<<"name">>,
		  translate:translate(Lang, <<"Commands">>)}],
	    children = []}
     | Acc];
get_local_identity(Acc, _From, _To, <<"ping">>, Lang) ->
    [#xmlel{name = <<"identity">>,
	    attrs =
		[{<<"category">>, <<"automation">>},
		 {<<"type">>, <<"command-node">>},
		 {<<"name">>, translate:translate(Lang, <<"Ping">>)}],
	    children = []}
     | Acc];
>>>>>>> upstream/master
get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

%% On disco info request to the ad-hoc node, return automation/command-list.
<<<<<<< HEAD
get_sm_identity(Acc, _From, _To, ?NS_ADHOC_s, Lang) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
      [?XMLATTR(<<"category">>, <<"automation">>),
       ?XMLATTR(<<"type">>, <<"command-list">>),
       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]} | Acc];

get_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

get_local_features(Acc, _From, _To, <<>>, _Lang) ->
=======
get_sm_identity(Acc, _From, _To, ?NS_COMMANDS, Lang) ->
    [#xmlel{name = <<"identity">>,
	    attrs =
		[{<<"category">>, <<"automation">>},
		 {<<"type">>, <<"command-list">>},
		 {<<"name">>,
		  translate:translate(Lang, <<"Commands">>)}],
	    children = []}
     | Acc];
get_sm_identity(Acc, _From, _To, _Node, _Lang) -> Acc.

%-------------------------------------------------------------------------

get_local_features(Acc, _From, _To, <<"">>, _Lang) ->
>>>>>>> upstream/master
    Feats = case Acc of
	      {result, I} -> I;
	      _ -> []
	    end,
<<<<<<< HEAD
    {result, Feats ++ [?NS_ADHOC_s]};

get_local_features(_Acc, _From, _To, ?NS_ADHOC_b, _Lang) ->
    %% override all lesser features...
    {result, []};

get_local_features(_Acc, _From, _To, <<"ping">>, _Lang) ->
    %% override all lesser features...
    {result, [?NS_ADHOC_s]};

=======
    {result, Feats ++ [?NS_COMMANDS]};
get_local_features(_Acc, _From, _To, ?NS_COMMANDS,
		   _Lang) ->
    {result, []};
get_local_features(_Acc, _From, _To, <<"ping">>,
		   _Lang) ->
    {result, [?NS_COMMANDS]};
>>>>>>> upstream/master
get_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

get_sm_features(Acc, _From, _To, <<"">>, _Lang) ->
    Feats = case Acc of
	      {result, I} -> I;
	      _ -> []
	    end,
<<<<<<< HEAD
    {result, Feats ++ [?NS_ADHOC_s]};

get_sm_features(_Acc, _From, _To, ?NS_ADHOC_s, _Lang) ->
    %% override all lesser features...
=======
    {result, Feats ++ [?NS_COMMANDS]};
get_sm_features(_Acc, _From, _To, ?NS_COMMANDS,
		_Lang) ->
>>>>>>> upstream/master
    {result, []};
get_sm_features(Acc, _From, _To, _Node, _Lang) -> Acc.

%-------------------------------------------------------------------------

<<<<<<< HEAD
process_local_iq(From, To, IQ_Rec) ->
    process_adhoc_request(From, To, IQ_Rec, adhoc_local_commands).

=======
process_local_iq(From, To, IQ) ->
    process_adhoc_request(From, To, IQ,
			  adhoc_local_commands).
>>>>>>> upstream/master

process_sm_iq(From, To, IQ_Rec) ->
    process_adhoc_request(From, To, IQ_Rec, adhoc_sm_commands).

<<<<<<< HEAD

process_adhoc_request(From, To, IQ_Rec, Hook) ->
    ?DEBUG("About to parse ~p...", [IQ_Rec]),
    case adhoc:parse_request(IQ_Rec) of
	{error, Error} ->
            exmpp_iq:error(IQ_Rec, Error);
	#adhoc_request{} = AdhocRequest ->
	    case ejabberd_hooks:run_fold(Hook, exmpp_jid:prep_domain(To), empty,
					 [From, To, AdhocRequest]) of
		ignore ->
		    ignore;
		empty ->
                    exmpp_iq:error(IQ_Rec, 'item-not-found');
		{error, Error} ->
                    exmpp_iq:error(IQ_Rec, Error);
		Command ->
                    exmpp_iq:result(IQ_Rec, Command)
	    end
    end.


ping_item(Acc, _From, To, Lang) ->
    Server = exmpp_jid:domain(To),
=======
process_adhoc_request(From, To,
		      #iq{sub_el = SubEl} = IQ, Hook) ->
    ?DEBUG("About to parse ~p...", [IQ]),
    case adhoc:parse_request(IQ) of
      {error, Error} ->
	  IQ#iq{type = error, sub_el = [SubEl, Error]};
      #adhoc_request{} = AdhocRequest ->
	  Host = To#jid.lserver,
	  case ejabberd_hooks:run_fold(Hook, Host, empty,
				       [From, To, AdhocRequest])
	      of
	    ignore -> ignore;
	    empty ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
	    {error, Error} ->
		IQ#iq{type = error, sub_el = [SubEl, Error]};
	    Command -> IQ#iq{type = result, sub_el = [Command]}
	  end
    end.

ping_item(Acc, _From, #jid{server = Server} = _To,
	  Lang) ->
>>>>>>> upstream/master
    Items = case Acc of
	      {result, I} -> I;
	      _ -> []
	    end,
<<<<<<< HEAD
    Nodes = [#xmlel{ns = ?NS_DISCO_INFO, name = 'item', attrs =
	      [?XMLATTR(<<"jid">>, Server),
	       ?XMLATTR(<<"node">>, <<"ping">>),
	       ?XMLATTR(<<"name">>, translate:translate(Lang, "Ping"))]}],
=======
    Nodes = [#xmlel{name = <<"item">>,
		    attrs =
			[{<<"jid">>, Server}, {<<"node">>, <<"ping">>},
			 {<<"name">>, translate:translate(Lang, <<"Ping">>)}],
		    children = []}],
>>>>>>> upstream/master
    {result, Items ++ Nodes}.

ping_command(_Acc, _From, _To,
<<<<<<< HEAD
	     #adhoc_request{lang = Lang,
			    node = "ping",
			    sessionid = _Sessionid,
			    action = Action} = Request) ->
    if 
	Action == ""; Action == "execute" ->
	    adhoc:produce_response(
	      Request,
	      #adhoc_response{status = completed,
			      notes = [{"info", translate:translate(
						  Lang,
						  "Pong")}]});
	true ->
	    {error, 'bad-request'}
=======
	     #adhoc_request{lang = Lang, node = <<"ping">>,
			    sessionid = _Sessionid, action = Action} =
		 Request) ->
    if Action == <<"">>; Action == <<"execute">> ->
	   adhoc:produce_response(Request,
				  #adhoc_response{status = completed,
						  notes =
						      [{<<"info">>,
							translate:translate(Lang,
									    <<"Pong">>)}]});
       true -> {error, ?ERR_BAD_REQUEST}
>>>>>>> upstream/master
    end;
ping_command(Acc, _From, _To, _Request) -> Acc.
