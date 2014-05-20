%%%----------------------------------------------------------------------
%%% File    : mod_http_bind.erl
%%% Author  : Stefan Strigler <steve@zeank.in-berlin.de>
%%% Purpose : Implementation of XMPP over BOSH (XEP-0206)
%%% Created : Tue Feb 20 13:15:52 CET 2007
%%%
%%%
<<<<<<< HEAD:src/web/mod_http_bind.erl
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
=======
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
>>>>>>> upstream/master:src/mod_http_bind.erl
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

%%%----------------------------------------------------------------------
%%% This module acts as a bridge to ejabberd_http_bind which implements
%%% the real stuff, this is to handle the new pluggable architecture for
%%% extending ejabberd's http service.
%%%----------------------------------------------------------------------

-module(mod_http_bind).

<<<<<<< HEAD:src/web/mod_http_bind.erl
=======
-author('steve@zeank.in-berlin.de').

>>>>>>> upstream/master:src/mod_http_bind.erl
%%-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([start/2, stop/1, process/2]).

-include_lib("exmpp/include/exmpp.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").
-include("http_bind.hrl").

-define(PROCNAME_MHB, ejabberd_mod_http_bind).

-include("http_bind.hrl").

-define(PROCNAME_MHB, ejabberd_mod_http_bind).

%% Duplicated from ejabberd_http_bind.
%% TODO: move to hrl file.
<<<<<<< HEAD:src/web/mod_http_bind.erl
-record(http_bind, {id, pid, to, hold, wait, process_delay, version}).
=======
-record(http_bind,
	{id, pid, to, hold, wait, process_delay, version}).
>>>>>>> upstream/master:src/mod_http_bind.erl

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

process([], #request{method = 'POST', data = <<>>}) ->
    ?DEBUG("Bad Request: no data", []),
<<<<<<< HEAD:src/web/mod_http_bind.erl
    {400, ?HEADER, #xmlel{name = h1, children =
			  [#xmlcdata{cdata = <<"400 Bad Request">>}]}};
process([], #request{method = 'POST',
                     data = Data,
                     ip = IP}) ->
    ?DEBUG("Incoming data: ~s", [Data]),
    ejabberd_http_bind:process_request(Data, IP);
process([], #request{method = 'GET',
                     data = []}) ->
    {200, ?HEADER, get_human_html_xmlel()};
process([], #request{method = 'OPTIONS',
                     data = []}) ->
    {200, ?OPTIONS_HEADER, []};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, ?HEADER, #xmlel{name = h1, children =
			  [#xmlcdata{cdata = <<"400 Bad Request">>}]}}.

get_human_html_xmlel() ->
    Heading = list_to_binary("ejabberd " ++ atom_to_list(?MODULE)),
    H = #xmlel{name = h1, children = [#xmlcdata{cdata = Heading}]},
    Par1 = #xmlel{name = p, children =
		  [#xmlcdata{cdata = <<"An implementation of ">>},
		   #xmlel{name = a,
			  attrs = [#xmlattr{name = <<"href">>, value = <<"http://xmpp.org/extensions/xep-0206.html">>}],
			  children = [#xmlcdata{cdata = <<"XMPP over BOSH (XEP-0206)">>}]
			 }
		  ]},
    Par2 = #xmlel{name = p, children =
		  [#xmlcdata{cdata = <<"This web page is only informative. "
				      "To use HTTP-Bind you need a Jabber/XMPP client that supports it.">>}
		  ]},
    #xmlel{name = html,
	   attrs = [#xmlattr{name = <<"xmlns">>, value= <<"http://www.w3.org/1999/xhtml">>}],
	   children =
	   [#xmlel{name = head, children = [#xmlel{name = title, children = [#xmlcdata{cdata = Heading}]}]},
	    #xmlel{name = body, children = [H, Par1, Par2]}]}.
=======
    {400, ?HEADER,
     #xmlel{name = <<"h1">>, children = [{xmlcdata, <<"400 Bad Request">>}]}};
process([],
	#request{method = 'POST', data = Data, ip = IP}) ->
    ?DEBUG("Incoming data: ~s", [Data]),
    ejabberd_http_bind:process_request(Data, IP);
process([], #request{method = 'GET', data = <<>>}) ->
    {200, ?HEADER, get_human_html_xmlel()};
process([], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, <<>>};
process([], #request{method = 'HEAD'}) ->
    {200, ?HEADER, <<>>};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, ?HEADER,
     #xmlel{name = <<"h1">>, children = [{xmlcdata, <<"400 Bad Request">>}]}}.

get_human_html_xmlel() ->
    Heading = <<"ejabberd ",
		(iolist_to_binary(atom_to_list(?MODULE)))/binary>>,
    #xmlel{name = <<"html">>,
	   attrs =
	       [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>}],
	   children =
	       [#xmlel{name = <<"head">>,
		       children =
			   [#xmlel{name = <<"title">>,
				   children = [{xmlcdata, Heading}]}]},
		#xmlel{name = <<"body">>,
		       children =
			   [#xmlel{name = <<"h1">>,
				   children = [{xmlcdata, Heading}]},
			    #xmlel{name = <<"p">>,
				   children =
				       [{xmlcdata, <<"An implementation of ">>},
					#xmlel{name = <<"a">>,
					       attrs =
						   [{<<"href">>,
						     <<"http://xmpp.org/extensions/xep-0206.html">>}],
					       children =
						   [{xmlcdata,
						     <<"XMPP over BOSH (XEP-0206)">>}]}]},
			    #xmlel{name = <<"p">>,
				   children =
				       [{xmlcdata,
					 <<"This web page is only informative. To "
					   "use HTTP-Bind you need a Jabber/XMPP "
					   "client that supports it.">>}]}]}]}.
>>>>>>> upstream/master:src/mod_http_bind.erl

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------
<<<<<<< HEAD:src/web/mod_http_bind.erl
start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, _Opts) ->
    setup_database(),
    Proc = gen_mod:get_module_proc(HostB, ?PROCNAME_MHB),
    ChildSpec =
        {Proc,
         {ejabberd_tmp_sup, start_link,
          [Proc, ejabberd_http_bind]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
=======
start(Host, _Opts) ->
    setup_database(),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME_MHB),
    ChildSpec = {Proc,
		 {ejabberd_tmp_sup, start_link,
		  [Proc, ejabberd_http_bind]},
		 permanent, infinity, supervisor, [ejabberd_tmp_sup]},
>>>>>>> upstream/master:src/mod_http_bind.erl
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME_MHB),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

setup_database() ->
    migrate_database(),
    mnesia:create_table(http_bind,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, http_bind)}]),
    mnesia:add_table_copy(http_bind, node(), ram_copies).

migrate_database() ->
    case catch mnesia:table_info(http_bind, attributes) of
        [id, pid, to, hold, wait, process_delay, version] ->
	    ok;
        _ ->
	    %% Since the stored information is not important, instead
	    %% of actually migrating data, let's just destroy the table
	    mnesia:delete_table(http_bind)
    end,
    case catch mnesia:table_info(http_bind, local_content) of
	false ->
	    mnesia:delete_table(http_bind);
	_ ->
	    ok
    end.
