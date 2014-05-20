%%%----------------------------------------------------------------------
%%% File    : mod_sic.erl
%%% Author  : Karim Gemayel <karim.gemayel@process-one.net>
%%% Purpose : XEP-0279 Server IP Check
%%% Created : 6 Mar 2010 by Karim Gemayel <karim.gemayel@process-one.net>
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

-module(mod_sic).
<<<<<<< HEAD
=======

>>>>>>> upstream/master
-author('karim.gemayel@process-one.net').

-behaviour(gen_mod).

<<<<<<< HEAD
-export([start/2,
	 stop/1,
	 process_local_iq/3,
	 process_sm_iq/3]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").
-include("ejabberd.hrl").

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_disco:register_feature(HostB, ?NS_SIC_0_s),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_SIC_0_s,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_SIC_0_s,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    HostB = list_to_binary(Host),
    mod_disco:unregister_feature(HostB, ?NS_SIC_0_s),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_SIC_0_s),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_SIC_0_s).


process_local_iq(From, _To, #iq{type = 'get'} = IQ) ->
    get_ip(From, IQ);

process_local_iq(_From, _To, #iq{type = 'set'} = IQ) ->
    exmpp_iq:error(IQ, 'not-allowed').


process_sm_iq(
  #jid{node = Node, domain = Domain} = From,
  #jid{node = Node, domain = Domain} = _To,
  #iq{type = 'get'} = IQ) ->
    get_ip(From, IQ);

process_sm_iq(_From, _To, #iq{type = 'get'} = IQ) ->
    exmpp_iq:error(IQ, 'forbidden');

process_sm_iq(_From, _To, #iq{type = 'set'} = IQ) ->
    exmpp_iq:error(IQ, 'not-allowed').

get_ip(From, IQ) ->
    case ejabberd_sm:get_user_ip(From) of
	{IP, _} when is_tuple(IP) ->
	    exmpp_iq:result(IQ,
			    #xmlel{
			      name = 'ip',
			      ns = ?NS_SIC_0_s,
			      children = [?XMLCDATA(list_to_binary(
						      inet_parse:ntoa(IP)))]
			     });
	_ ->
	    exmpp_iq:error(IQ, 'internal-server-error')
=======
-export([start/2, stop/1, process_local_iq/3,
	 process_sm_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(NS_SIC, <<"urn:xmpp:sic:0">>).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_SIC, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_SIC, ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_SIC),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_SIC).

process_local_iq(#jid{user = User, server = Server,
		      resource = Resource},
		 _To, #iq{type = get, sub_el = _SubEl} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_local_iq(_From, _To,
		 #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_sm_iq(#jid{user = User, server = Server,
		   resource = Resource},
	      #jid{user = User, server = Server},
	      #iq{type = get, sub_el = _SubEl} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_sm_iq(_From, _To,
	      #iq{type = get, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]};
process_sm_iq(_From, _To,
	      #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

get_ip({User, Server, Resource},
       #iq{sub_el =
	       #xmlel{name = Name, attrs = Attrs} = SubEl} =
	   IQ) ->
    case ejabberd_sm:get_user_ip(User, Server, Resource) of
      {IP, _} when is_tuple(IP) ->
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = Name, attrs = Attrs,
			    children =
				[{xmlcdata,
				  iolist_to_binary(jlib:ip_to_list(IP))}]}]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
>>>>>>> upstream/master
    end.
