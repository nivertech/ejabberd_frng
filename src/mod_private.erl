%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / private_storage
%%%  usns = {Username::string(), Host::string(), Namespace::string()}
%%%  xml = xmlelement()
%%%
%%% 2.1.x / odbc / private_storage
%%%  username = varchar250
%%%  namespace = varchar250
%%%  data = text
%%%
%%% 3.0.0-prealpha / mnesia / private_storage
%%%  usns = {Username::binary(), Host::binary(), Namespace::atom()}
%%%  xml = xmlel()
%%%
%%% 3.0.0-prealpha / odbc / private_storage
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / private_storage
%%%  user_host_ns = {Username::binary(), Host::binary(), Namespace::atom()}
%%%  xml = xmlel()
%%%
%%% 3.0.0-alpha / odbc / private_storage
%%%  user = varchar
%%%  host = varchar
%%%  ns = varchar250
%%%  xml = text

-module(mod_private).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_sm_iq/3, import/3,
	 remove_user/2, get_data/2, export/1, import/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD

%% TODO: usns instead of user_host_ns requires no migration
-record(private_storage, {user_host_ns, xml}).

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, HostB, private_storage,
			     [{disc_only_copies, [node()]},
			      {odbc_host, HostB},
			      {attributes, record_info(fields, private_storage)},
			      {types, [{user_host_ns, {binary, binary, atom}}, {xml, xmlel}]}]),
    update_table(HostB, Backend),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_PRIVATE,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_PRIVATE).


process_sm_iq(From, To, #iq{type = Type} = IQ_Rec) ->
    case check_packet(From, To, IQ_Rec) of
	ok ->
	    case Type of
		set ->
		    process_iq_set(From, To, IQ_Rec);
		get ->
		    process_iq_get(From, To, IQ_Rec)
	    end;
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end.

process_iq_get(From, _To, #iq{payload = SubEl} = IQ_Rec) ->
    LUser = exmpp_jid:prep_node(From),
    LServer = exmpp_jid:prep_domain(From),
    case catch get_data(LUser,
			LServer,
			exmpp_xml:get_child_elements(SubEl)) of
	{'EXIT', _Reason} ->
            exmpp_iq:error(IQ_Rec, 'internal-server-error');
	Res ->
	    exmpp_iq:result(IQ_Rec, #xmlel{ns = ?NS_PRIVATE,
					   name = 'query',
					   children = Res})
    end.


process_iq_set(From, _To, #iq{payload = SubEl} = IQ_Rec) ->
    LUser = exmpp_jid:prep_node(From),
    LServer = exmpp_jid:prep_domain(From),
    F = fun() ->
        lists:foreach(
          fun(El) ->
              set_data(LUser, LServer, El)
          end, exmpp_xml:get_child_elements(SubEl))
    end,
    gen_storage:transaction(LServer, private_storage, F),
    exmpp_iq:result(IQ_Rec).


check_packet(From, To, IQ_Rec) ->
    check_packet(From, To, IQ_Rec, [ fun check_domain/3,
				     fun check_user/3,
				     fun check_ns/3]).
check_packet(_From, _To, _IQ_Rec, []) ->
    ok;
check_packet(From, To, IQ_Rec, [F | R]) ->
    case F(From, To, IQ_Rec) of
	{error, _} = Error -> Error;
	ok -> check_packet(From, To, IQ_Rec, R)
    end.

check_domain(From, _To, _IQ_Rec) ->
    LServer = exmpp_jid:prep_domain_as_list(From),
    case ?IS_MY_HOST(LServer) of
	true -> ok;
	false -> {error, 'not-allowed'}
    end.

% the iq can't be directed to another jid
check_user(From, To, _IQ_Rec) ->
    case exmpp_jid:bare_compare(From, To) of
	true -> ok;
	false -> {error, 'forbidden'}
    end.

%there must be at least one child, and every child should have
%a namespace specified (reject if the namespace is jabber:iq:private,
%the same than the parent element).
check_ns(_From, _To, #iq{payload = SubEl}) ->
    case exmpp_xml:get_child_elements(SubEl) of
	[] ->
	    {error, 'not-acceptable'};
	Children ->
	    case lists:any(fun(Child) ->
			       exmpp_xml:get_ns_as_atom(Child) =:= ?NS_PRIVATE
			   end, Children) of
		true -> {error, 'not-acceptable'};
		false -> ok
	    end
    end.

%% The xml is stored as xmlel() in mnesia, but as text in odbc
set_data(LUser, LServer, El) ->
    XMLNS = exmpp_xml:get_ns_as_atom(El),
    gen_storage:write(LServer,
		      #private_storage{user_host_ns = {LUser, LServer, XMLNS},
				       xml = El}).

get_data(LUser, LServer, Els) ->
    get_data(LUser, LServer, Els, []).

get_data(_LUser, _LServer, [], Res) ->
    lists:reverse(Res);
get_data(LUser, LServer, [El | Els], Res) ->
    XMLNS = exmpp_xml:get_ns_as_atom(El),
    case gen_storage:dirty_read(LServer, private_storage, {LUser, LServer, XMLNS}) of
	[R] ->
	    get_data(LUser, LServer, Els,
	      [R#private_storage.xml | Res]);
	[] ->
	    get_data(LUser, LServer, Els,
	      [El | Res])
    end.

remove_user(User, Server) 
        when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	F = fun() ->
		Records = gen_storage:select(LServer, private_storage,
					     [{'=', user_host_ns, {LUser, LServer, '_'}}]),
		lists:foreach(
		  fun(#private_storage{user_host_ns = USNS}) ->
			  gen_storage:delete(LServer, {private_storage, USNS})
		     end, Records)
	    end,
	gen_storage:transaction(LServer, private_storage, F)
    catch
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_table(global, Storage) ->
    [update_table(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_table(Host, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      Host, private_storage,
      [{private_storage, [usns, xml],
	fun({private_storage, {User, Server, NS}, Xml}) ->
		U1 = list_to_binary(User),
		S1 = list_to_binary(Server),
		NS1 = list_to_atom(NS),
		El1 = exmpp_xml:xmlelement_to_xmlel(Xml, [?NS_PRIVATE],
						    [{?NS_XMPP, ?NS_XMPP_pfx}]),
		#private_storage{user_host_ns = {U1, S1, NS1},
				 xml = El1}
	end}]);

update_table(Host, odbc) ->
    gen_storage_migration:migrate_odbc(
      Host, [private_storage],
      [{"private_storage", ["username", "namespace", "data"],
	fun(_, Username, Namespace, Data) ->
		[#private_storage{user_host_ns = {Username, Host, Namespace},
				  xml = Data}]
	end}]).
=======
-include("logger.hrl").

-include("jlib.hrl").

-record(private_storage,
        {usns = {<<"">>, <<"">>, <<"">>} :: {binary(), binary(), binary() |
                                             '$1' | '_'},
         xml = #xmlel{} :: xmlel() | '_' | '$1'}).

-define(Xmlel_Query(Attrs, Children),
	#xmlel{name = <<"query">>, attrs = Attrs,
	       children = Children}).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    case gen_mod:db_type(Opts) of
      mnesia ->
	  mnesia:create_table(private_storage,
			      [{disc_only_copies, [node()]},
			       {attributes,
				record_info(fields, private_storage)}]),
	  update_table();
      _ -> ok
    end,
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PRIVATE, ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PRIVATE).

process_sm_iq(#jid{luser = LUser, lserver = LServer},
	      #jid{luser = LUser, lserver = LServer}, IQ)
    when IQ#iq.type == set ->
    case IQ#iq.sub_el of
      #xmlel{name = <<"query">>, children = Xmlels} ->
	  case filter_xmlels(Xmlels) of
	    [] ->
		IQ#iq{type = error,
		      sub_el = [IQ#iq.sub_el, ?ERR_NOT_ACCEPTABLE]};
	    Data ->
		DBType = gen_mod:db_type(LServer, ?MODULE),
		F = fun () ->
			    lists:foreach(fun (Datum) ->
						  set_data(LUser, LServer,
							   Datum, DBType)
					  end,
					  Data)
		    end,
		case DBType of
		  odbc -> ejabberd_odbc:sql_transaction(LServer, F);
		  mnesia -> mnesia:transaction(F)
		end,
		IQ#iq{type = result, sub_el = []}
	  end;
      _ ->
	  IQ#iq{type = error,
		sub_el = [IQ#iq.sub_el, ?ERR_NOT_ACCEPTABLE]}
    end;
%%
process_sm_iq(#jid{luser = LUser, lserver = LServer},
	      #jid{luser = LUser, lserver = LServer}, IQ)
    when IQ#iq.type == get ->
    case IQ#iq.sub_el of
      #xmlel{name = <<"query">>, attrs = Attrs,
	     children = Xmlels} ->
	  case filter_xmlels(Xmlels) of
	    [] ->
		IQ#iq{type = error,
		      sub_el = [IQ#iq.sub_el, ?ERR_BAD_FORMAT]};
	    Data ->
		case catch get_data(LUser, LServer, Data) of
		  {'EXIT', _Reason} ->
		      IQ#iq{type = error,
			    sub_el =
				[IQ#iq.sub_el, ?ERR_INTERNAL_SERVER_ERROR]};
		  Storage_Xmlels ->
		      IQ#iq{type = result,
			    sub_el = [?Xmlel_Query(Attrs, Storage_Xmlels)]}
		end
	  end;
      _ ->
	  IQ#iq{type = error,
		sub_el = [IQ#iq.sub_el, ?ERR_BAD_FORMAT]}
    end;
%%
process_sm_iq(_From, _To, IQ) ->
    IQ#iq{type = error,
	  sub_el = [IQ#iq.sub_el, ?ERR_FORBIDDEN]}.

filter_xmlels(Xmlels) -> filter_xmlels(Xmlels, []).

filter_xmlels([], Data) -> lists:reverse(Data);
filter_xmlels([#xmlel{attrs = Attrs} = Xmlel | Xmlels],
	      Data) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
      <<"">> -> [];
      XmlNS -> filter_xmlels(Xmlels, [{XmlNS, Xmlel} | Data])
    end;
filter_xmlels([_ | Xmlels], Data) ->
    filter_xmlels(Xmlels, Data).

set_data(LUser, LServer, {XmlNS, Xmlel}, mnesia) ->
    mnesia:write(#private_storage{usns =
				      {LUser, LServer, XmlNS},
				  xml = Xmlel});
set_data(LUser, LServer, {XMLNS, El}, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    LXMLNS = ejabberd_odbc:escape(XMLNS),
    SData = ejabberd_odbc:escape(xml:element_to_binary(El)),
    odbc_queries:set_private_data(LServer, Username, LXMLNS,
				  SData).

get_data(LUser, LServer, Data) ->
    get_data(LUser, LServer,
	     gen_mod:db_type(LServer, ?MODULE), Data, []).

get_data(_LUser, _LServer, _DBType, [],
	 Storage_Xmlels) ->
    lists:reverse(Storage_Xmlels);
get_data(LUser, LServer, mnesia,
	 [{XmlNS, Xmlel} | Data], Storage_Xmlels) ->
    case mnesia:dirty_read(private_storage,
			   {LUser, LServer, XmlNS})
	of
      [#private_storage{xml = Storage_Xmlel}] ->
	  get_data(LUser, LServer, mnesia, Data,
		   [Storage_Xmlel | Storage_Xmlels]);
      _ ->
	  get_data(LUser, LServer, mnesia, Data,
		   [Xmlel | Storage_Xmlels])
    end;
get_data(LUser, LServer, odbc, [{XMLNS, El} | Els],
	 Res) ->
    Username = ejabberd_odbc:escape(LUser),
    LXMLNS = ejabberd_odbc:escape(XMLNS),
    case catch odbc_queries:get_private_data(LServer,
					     Username, LXMLNS)
	of
      {selected, [<<"data">>], [[SData]]} ->
	  case xml_stream:parse_element(SData) of
	    Data when is_record(Data, xmlel) ->
		get_data(LUser, LServer, odbc, Els, [Data | Res])
	  end;
      %% MREMOND: I wonder when the query could return a vcard ?
      {selected, [<<"vcard">>], []} ->
	  get_data(LUser, LServer, odbc, Els, [El | Res]);
      _ -> get_data(LUser, LServer, odbc, Els, [El | Res])
    end.


get_data(LUser, LServer) ->
    get_all_data(LUser, LServer,
                 gen_mod:db_type(LServer, ?MODULE)).

get_all_data(LUser, LServer, mnesia) ->
    lists:flatten(
      mnesia:dirty_select(private_storage,
                          [{#private_storage{usns = {LUser, LServer, '_'},
                                             xml = '$1'},
                            [], ['$1']}]));
get_all_data(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_private_data(LServer, Username) of
        {selected, [<<"namespace">>, <<"data">>], Res} ->
            lists:flatmap(
              fun([_, SData]) ->
                      case xml_stream:parse_element(SData) of
                          #xmlel{} = El ->
                              [El];
                          _ ->
                              []
                      end
              end, Res);
        _ ->
            []
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    remove_user(LUser, LServer,
		gen_mod:db_type(Server, ?MODULE)).

remove_user(LUser, LServer, mnesia) ->
    F = fun () ->
		Namespaces = mnesia:select(private_storage,
					   [{#private_storage{usns =
								  {LUser,
								   LServer,
								   '$1'},
							      _ = '_'},
					     [], ['$$']}]),
		lists:foreach(fun ([Namespace]) ->
				      mnesia:delete({private_storage,
						     {LUser, LServer,
						      Namespace}})
			      end,
			      Namespaces)
	end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_user_private_storage(LServer,
					  Username).

update_table() ->
    Fields = record_info(fields, private_storage),
    case mnesia:table_info(private_storage, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            private_storage, Fields, set,
            fun(#private_storage{usns = {U, _, _}}) -> U end,
            fun(#private_storage{usns = {U, S, NS}, xml = El} = R) ->
                    R#private_storage{usns = {iolist_to_binary(U),
                                              iolist_to_binary(S),
                                              iolist_to_binary(NS)},
                                      xml = xml:to_xmlel(El)}
            end);
      _ ->
	  ?INFO_MSG("Recreating private_storage table", []),
	  mnesia:transform_table(private_storage, ignore, Fields)
    end.

export(_Server) ->
    [{private_storage,
      fun(Host, #private_storage{usns = {LUser, LServer, XMLNS},
                                 xml = Data})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              LXMLNS = ejabberd_odbc:escape(XMLNS),
              SData =
                  ejabberd_odbc:escape(xml:element_to_binary(Data)),
              odbc_queries:set_private_data_sql(Username, LXMLNS,
                                                SData);
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, namespace, data from private_storage;">>,
      fun([LUser, XMLNS, XML]) ->
              El = #xmlel{} = xml_stream:parse_element(XML),
              #private_storage{usns = {LUser, LServer, XMLNS},
                               xml = El}
      end}].

import(_LServer, mnesia, #private_storage{} = PS) ->
    mnesia:dirty_write(PS);
import(_, _, _) ->
    pass.
>>>>>>> upstream/master
