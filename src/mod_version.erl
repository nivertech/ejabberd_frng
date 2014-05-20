%%%----------------------------------------------------------------------
%%% File    : mod_version.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XEP-0092: Software Version
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_version).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3]).

<<<<<<< HEAD
-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
=======
-include("ejabberd.hrl").
-include("logger.hrl").
>>>>>>> upstream/master

-include("jlib.hrl").

<<<<<<< HEAD
start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_SOFT_VERSION,
				  ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, list_to_binary(Host), ?NS_SOFT_VERSION).


process_local_iq(_From, To, #iq{type = Type} = IQ_Rec) ->
    case Type of
	set ->
	    exmpp_iq:error(IQ_Rec, 'not-allowed');
	get ->
	    Host = exmpp_jid:domain_as_list(To),
	    OS = case gen_mod:get_module_opt(Host, ?MODULE, show_os, true) of
		     true -> [get_os()];
		     false -> []
		 end,
	    R = #xmlel{ns = ?NS_SOFT_VERSION, name = 'query',
		       children = [ exmpp_xml:set_cdata(#xmlel{ns = ?NS_SOFT_VERSION,
							       name = 'name'},
							<<"ejabberd">>),
				    exmpp_xml:set_cdata(#xmlel{ns = ?NS_SOFT_VERSION,
							       name = 'version'},
							?VERSION) | OS]},
	    exmpp_iq:result(IQ_Rec, R)
=======
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_VERSION, ?MODULE, process_local_iq,
				  IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_VERSION).

process_local_iq(_From, To,
		 #iq{id = _ID, type = Type, xmlns = _XMLNS,
		     sub_el = SubEl} =
		     IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  Host = To#jid.lserver,
	  OS = case gen_mod:get_module_opt(Host, ?MODULE, show_os,
                                           fun(B) when is_boolean(B) -> B end,
					   true)
		   of
		 true -> [get_os()];
		 false -> []
	       end,
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_VERSION}],
			    children =
				[#xmlel{name = <<"name">>, attrs = [],
					children =
					    [{xmlcdata, <<"ejabberd">>}]},
				 #xmlel{name = <<"version">>, attrs = [],
					children = [{xmlcdata, ?VERSION}]}]
				  ++ OS}]}
>>>>>>> upstream/master
    end.

get_os() ->
    {Osfamily, Osname} = os:type(),
    OSType = list_to_binary([atom_to_list(Osfamily), $/, atom_to_list(Osname)]),
    OSVersion = case os:version() of
		  {Major, Minor, Release} ->
		      iolist_to_binary(io_lib:format("~w.~w.~w",
						     [Major, Minor, Release]));
		  VersionString -> VersionString
		end,
<<<<<<< HEAD
    OS = OSType ++ " " ++ OSVersion,
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_SOFT_VERSION, name = 'os'}, OS).
=======
    OS = <<OSType/binary, " ", OSVersion/binary>>,
    #xmlel{name = <<"os">>, attrs = [],
	   children = [{xmlcdata, OS}]}.
>>>>>>> upstream/master
