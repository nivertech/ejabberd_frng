%%%----------------------------------------------------------------------
%%% File    : ejd2odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Export some mnesia tables to SQL DB
%%% Created : 22 Aug 2005 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejd2odbc).

-author('alexey@process-one.net').

<<<<<<< HEAD
%% External exports
-export([export_passwd/2,
	 export_roster/2,
	 export_roster_group/2,
	 export_offline/2,
	 export_last/2,
	 export_vcard/2,
	 export_vcard_search/2,
	 export_private_storage/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_roster.hrl").

-record(offline_msg, {user_host, timestamp, expire, from, to, packet}).
-record(last_activity, {user_host, timestamp, status}).
-record(vcard, {user_host, vcard}).
-record(vcard_search, {user_host,
		       user,     luser,
		       fn,	 lfn,
		       family,	 lfamily,
		       given,	 lgiven,
		       middle,	 lmiddle,
		       nickname, lnickname,
		       bday,	 lbday,
		       ctry,	 lctry,
		       locality, llocality,
		       email,	 lemail,
		       orgname,	 lorgname,
		       orgunit,	 lorgunit
		      }).
-record(private_storage, {user_host_ns, xml}).
=======
-include("logger.hrl").

-export([export/2, export/3, import_file/2, import/2, import/3]).

-define(MAX_RECORDS_PER_TRANSACTION, 100).
>>>>>>> upstream/master

-record(dump, {fd, cont = start}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%% How to use:
%%% A table can be converted from Mnesia to an ODBC database by calling
%%% one of the API function with the following parameters:
%%% - Server is the server domain you want to convert
%%% - Output can be either odbc to export to the configured relational
%%%   database or "Filename" to export to text file.

<<<<<<< HEAD
export_passwd(Server, Output) ->
    export_common(
      Server, passwd, Output,
      fun(Host, {passwd, {LUser, LServer}, Password} = _R)
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      Pass = ejabberd_odbc:escape(Password),
	      ["delete from users where host='", Server, "' and username='", Username ,"';\n"
	       "insert into users(host, username, password) "
	       "values ('", Server, "', '", Username, "', '", Pass, "');\n"];
	 (_Host, _R) ->
	      []
      end).

export_roster(ServerS, Output) ->
    Server = list_to_binary(ServerS),
    export_common(
      Server, rosteritem, Output,
      fun(Host, #rosteritem{user_host_jid = {LUser, LServer, {N, D, Res} = _LJID}} = R)
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      SJID = ejabberd_odbc:escape(exmpp_jid:to_list(N, D, Res)),
	      ItemVals = record_to_string(R, ServerS),
	      ["delete from rosterusers "
	       "      where username='", Username, "' "
	       "        and jid='", SJID, "';\n"
	       "insert into rosterusers("
	       "              host, username, jid, nick, "
	       "              subscription, ask, askmessage, "
	       "              server, subscribe, type) "
	       " values ", ItemVals, ";\n"];
	 (_Host, _R) ->
	      []
      end).

export_roster_group(ServerS, Output) ->
    Server = list_to_binary(ServerS),
    export_common(
      Server, rostergroup, Output,
      fun(Host, #rostergroup{user_host_jid = {_LUser, LServer, _LJID}} = R)
	 when LServer == Host ->
	      ItemGroup = group_to_string(R, ServerS),
	      [%%"delete from rostergroups"
	       %%" where username='", Username, "'"
	       %%" and host='", ServerS, "'"
	       %%" and jid='", SJID, "';"
	       "insert into rostergroups(host, username, jid, grp)"
	       " values ", ItemGroup, ";\n"];
	 (_Host, _R) ->
	      []
      end).

export_offline(Server, Output) ->
    export_common(
      Server, offline_msg, Output,
      fun(Host, #offline_msg{user_host = {LUser, LServer},
			     timestamp = TimeStamp,
			     from = From,
			     to = To,
			     packet = PacketString})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      [Packet] = exmpp_xml:parse_document(PacketString, [names_as_atom]), 
	      Packet0 = exmpp_stanza:set_jids(Packet,
		exmpp_jid:to_list(From),
		exmpp_jid:to_list(To)),
	      Packet0b = exmpp_xml:append_child(Packet0,
			   jlib:timestamp_to_xml(
			      calendar:gregorian_seconds_to_datetime(TimeStamp), 
			      utc,
			      exmpp_jid:make("", Server, ""),
			      "Offline Storage")),
	      %% TODO: Delete the next three lines once XEP-0091 is Obsolete
	      Packet1 = exmpp_xml:append_child(Packet0b,
		jlib:timestamp_to_xml(
                  calendar:gregorian_seconds_to_datetime(TimeStamp))),
	      XML =
		  ejabberd_odbc:escape(
		    exmpp_xml:document_to_list(Packet1)),
	      ["insert into spool(host, username, xml) "
	       "values ('", Server, "', '", Username, "', '",
	       XML,
	       "');\n"];
	 (_Host, _R) ->
	      []
      end).

export_last(ServerS, Output) ->
    Server = list_to_binary(ServerS),
    export_common(
      Server, last_activity, Output,
      fun(Host, #last_activity{user_host = {LUser, LServer},
			       timestamp = TimeStamp,
			       status = Status})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      Seconds = ejabberd_odbc:escape(integer_to_list(TimeStamp)),
	      State = ejabberd_odbc:escape(Status),
	      ["delete from last where host='", ServerS, "' and username='", Username, "';\n"
	       "insert into last(username, seconds, state) "
	       "values ('", Username, "', '", Seconds, "', '", State, "');\n"];
	 (_Host, _R) ->
	      []
      end).

export_vcard(Server, Output) ->
    export_common(
      Server, vcard, Output,
      fun(Host, #vcard{user_host = {LUser, LServer},
		       vcard = VCARD})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      SVCARD = ejabberd_odbc:escape(
			 exmpp_xml:document_to_list(VCARD)),
	      ["delete from vcard where host='", Server,"' and username='", Username, "';\n"
	       "insert into vcard(host, username, vcard) "
	       "values ('", Server, "', '", Username, "', '", SVCARD, "');\n"];
	 (_Host, _R) ->
	      []
      end).

export_vcard_search(Server, Output) ->
    export_common(
      Server, vcard_search, Output,
      fun(Host, #vcard_search{user      = {User, LServer},
			      luser     = LUser,
			      fn        = FN,       lfn        = LFN,       
			      family    = Family,   lfamily    = LFamily,   
			      given     = Given,    lgiven     = LGiven,    
			      middle    = Middle,   lmiddle    = LMiddle,   
			      nickname  = Nickname, lnickname  = LNickname, 
			      bday      = BDay,     lbday      = LBDay,     
			      ctry      = CTRY,     lctry      = LCTRY,     
			      locality  = Locality, llocality  = LLocality, 
			      email     = EMail,    lemail     = LEMail,    
			      orgname   = OrgName,  lorgname   = LOrgName,  
			      orgunit   = OrgUnit,  lorgunit   = LOrgUnit   
			     })
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(User),
	      LUsername = ejabberd_odbc:escape(LUser),
=======
modules() ->
    [ejabberd_auth,
     mod_announce,
     mod_caps,
     mod_irc,
     mod_last,
     mod_muc,
     mod_offline,
     mod_privacy,
     mod_private,
     mod_pubsub,
     mod_roster,
     mod_shared_roster,
     mod_vcard,
     mod_vcard_xupdate].

export(Server, Output) ->
    LServer = jlib:nameprep(iolist_to_binary(Server)),
    Modules = modules(),
    IO = prepare_output(Output),
    lists:foreach(
      fun(Module) ->
              export(LServer, IO, Module)
      end, Modules),
    close_output(Output, IO).

export(Server, Output, Module) ->
    LServer = jlib:nameprep(iolist_to_binary(Server)),
    IO = prepare_output(Output),
    lists:foreach(
      fun({Table, ConvertFun}) ->
              export(LServer, Table, IO, ConvertFun)
      end, Module:export(Server)),
    close_output(Output, IO).

import_file(Server, FileName) when is_binary(FileName) ->
    import(Server, binary_to_list(FileName));
import_file(Server, FileName) ->
    case disk_log:open([{name, make_ref()},
                        {file, FileName},
                        {mode, read_only}]) of
        {ok, Fd} ->
            LServer = jlib:nameprep(Server),
            Mods = [{Mod, gen_mod:db_type(LServer, Mod)}
                    || Mod <- modules(), gen_mod:is_loaded(LServer, Mod)],
            AuthMods = case lists:member(ejabberd_auth_internal,
                                         ejabberd_auth:auth_modules(LServer)) of
                           true ->
                               [{ejabberd_auth, mnesia}];
                           false ->
                               []
                       end,
            import_dump(LServer, AuthMods ++ Mods, #dump{fd = Fd});
        Err ->
            exit(Err)
    end.
>>>>>>> upstream/master

import(Server, Output) ->
    import(Server, Output, [{fast, true}]).

<<<<<<< HEAD
	      ["delete from vcard_search where host='", Server, "' and lusername='", LUsername, "';\n"
	       "insert into vcard_search("
	       "        username, lusername, fn, lfn, family, lfamily,"
	       "        given, lgiven, middle, lmiddle, nickname, lnickname,"
	       "        bday, lbday, ctry, lctry, locality, llocality,"
	       "        email, lemail, orgname, lorgname, orgunit, lorgunit)"
	       "values (",
	       "        '", Username, "', '",  LUsername, "'," 
	       "        '", SFN,       "', '", SLFN,       "'," 
	       "        '", SFamily,   "', '", SLFamily,   "',"
	       "        '", SGiven,    "', '", SLGiven,	 "',"
	       "        '", SMiddle,   "', '", SLMiddle,   "',"
	       "        '", SNickname, "', '", SLNickname, "',"
	       "        '", SBDay,     "', '", SLBDay,	 "',"
	       "        '", SCTRY,     "', '", SLCTRY,	 "',"
	       "        '", SLocality, "', '", SLLocality, "',"
	       "        '", SEMail,    "', '", SLEMail,	 "',"
	       "        '", SOrgName,  "', '", SLOrgName,  "',"
	       "        '", SOrgUnit,  "', '", SLOrgUnit,  "');\n"];
	 (_Host, _R) ->
	      []
      end).

export_private_storage(ServerS, Output) ->
    Server = list_to_binary(ServerS),
    export_common(
      Server, private_storage, Output,
      fun(Host, #private_storage{user_host_ns = {LUser, LServer, XMLNS},
				 xml = Data})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
      	      LXMLNS = ejabberd_odbc:escape(atom_to_list(XMLNS)),
	      SData = ejabberd_odbc:escape(
			exmpp_xml:document_to_list(Data)),
      	      odbc_queries:set_private_data_sql(LServer, Username, LXMLNS, SData);
	 (_Host, _R) ->
      	      []
      end).
=======
import(Server, Output, Opts) ->
    LServer = jlib:nameprep(iolist_to_binary(Server)),
    Modules = modules(),
    IO = prepare_output(Output, disk_log),
    lists:foreach(
      fun(Module) ->
              import(LServer, IO, Opts, Module)
      end, Modules),
    close_output(Output, IO).

import(Server, Output, Opts, Module) ->
    LServer = jlib:nameprep(iolist_to_binary(Server)),
    IO = prepare_output(Output, disk_log),
    lists:foreach(
      fun({SelectQuery, ConvertFun}) ->
              import(LServer, SelectQuery, IO, ConvertFun, Opts)
      end, Module:import(Server)),
    close_output(Output, IO).
>>>>>>> upstream/master

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
export(LServer, Table, IO, ConvertFun) ->
    F = fun () ->
                mnesia:read_lock_table(Table),
                {_N, SQLs} =
                    mnesia:foldl(
                      fun(R, {N, SQLs} = Acc) ->
                              case ConvertFun(LServer, R) of
                                  [] ->
                                      Acc;
                                  SQL ->
                                      if N < (?MAX_RECORDS_PER_TRANSACTION) - 1 ->
                                              {N + 1, [SQL | SQLs]};
                                         true ->
                                              output(LServer,
                                                     Table, IO,
                                                     flatten([SQL | SQLs])),
                                              {0, []}
                                      end
                              end
                      end,
                      {0, []}, Table),
                output(LServer, Table, IO, flatten(SQLs))
        end,
    mnesia:transaction(F).

output(_LServer, _Table, _IO, []) ->
    ok;
output(LServer, _Table, odbc, SQLs) ->
    ejabberd_odbc:sql_transaction(LServer, SQLs);
output(_LServer, Table, Fd, SQLs) ->
    file:write(Fd, ["-- \n-- Mnesia table: ", atom_to_list(Table),
                    "\n--\n", SQLs]).

import(LServer, SelectQuery, IO, ConvertFun, Opts) ->
    F = case proplists:get_bool(fast, Opts) of
            true ->
                fun() ->
                        case ejabberd_odbc:sql_query_t(SelectQuery) of
                            {selected, _, Rows} ->
                                lists:foldl(fun process_sql_row/2,
                                            {IO, ConvertFun, undefined}, Rows);
                            Err ->
                                erlang:error(Err)
                        end
                end;
            false ->
                fun() ->
                        ejabberd_odbc:sql_query_t(
                          [iolist_to_binary(
                             [<<"declare c cursor for ">>, SelectQuery])]),
                        fetch(IO, ConvertFun, undefined)
                end
        end,
    ejabberd_odbc:sql_transaction(LServer, F).

fetch(IO, ConvertFun, PrevRow) ->
    case ejabberd_odbc:sql_query_t([<<"fetch c;">>]) of
        {selected, _, [Row]} ->
            process_sql_row(Row, {IO, ConvertFun, PrevRow}),
            fetch(IO, ConvertFun, Row);
        {selected, _, []} ->
            ok;
        Err ->
            erlang:error(Err)
    end.

<<<<<<< HEAD
export_common(Server, Table, Output, ConvertFun) ->
    IO = case Output of
	     odbc ->
		 odbc;
	     _ ->
		 {ok, IODevice} = file:open(Output, [write, raw]),
		 IODevice
	 end,
    mnesia:transaction(
      fun() ->
	      mnesia:read_lock_table(Table),
	      LServer = exmpp_stringprep:nameprep(Server),
	      {_N, SQLs} =
		  mnesia:foldl(
		    fun(R, {N, SQLs} = Acc) ->
			    case ConvertFun(LServer, R) of
				[] ->
				    Acc;
				SQL ->
				    if
					N < ?MAX_RECORDS_PER_TRANSACTION - 1 ->
					    {N + 1, [SQL | SQLs]};
					true ->
					    %% Execute full SQL transaction
					    output(LServer, IO,
						   ["begin;\n",
						    lists:reverse([SQL | SQLs]),
						    "commit"]),
					    {0, []}
				    end
			    end
		    end, {0, []}, Table),
		  %% Execute SQL transaction with remaining records
	      output(LServer, IO,
		     ["begin;\n",
		      lists:reverse(SQLs),
		      "commit"])
      end).
=======
process_sql_row(Row, {IO, ConvertFun, PrevRow}) when Row == PrevRow ->
    %% Avoid calling ConvertFun with the same input
    {IO, ConvertFun, Row};
process_sql_row(Row, {IO, ConvertFun, _PrevRow}) ->
    case catch ConvertFun(Row) of
        {'EXIT', _} = Err ->
            ?ERROR_MSG("failed to convert ~p: ~p", [Row, Err]);
        Term ->
            ok = disk_log:log(IO#dump.fd, Term)
    end,
    {IO, ConvertFun, Row}.
>>>>>>> upstream/master

import_dump(LServer, Mods, #dump{fd = Fd, cont = Cont}) ->
    case disk_log:chunk(Fd, Cont) of
        {NewCont, Terms} ->
            import_terms(LServer, Mods, Terms),
            import_dump(LServer, Mods, #dump{fd = Fd, cont = NewCont});
        eof ->
            ok;
        Err ->
            exit(Err)
    end.

<<<<<<< HEAD
record_to_string(#rosteritem{user_host_jid = {User, _Server, {N, D, R} = _JID},
			 name = Name,
			 subscription = Subscription,
			 ask = Ask,
			 askmessage = AskMessage}, ServerS) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(exmpp_jid:to_list(N, D, R)),
    Nick = ejabberd_odbc:escape(Name),
    SSubscription = case Subscription of
			both -> "B";
			to   -> "T";
			from -> "F";
			none -> "N"
		    end,
    SAsk = case Ask of
	       subscribe   -> "S";
	       unsubscribe -> "U";
	       both	   -> "B";
	       out	   -> "O";
	       in	   -> "I";
	       none	   -> "N"
	   end,
    SAskMessage =
	case catch ejabberd_odbc:escape(
		     binary_to_list(list_to_binary([AskMessage]))) of
	    {'EXIT', _Reason} ->
		[];
	    SAM ->
		SAM
	end,
    ["("
     "'", ServerS, "',"
     "'", Username, "',"
     "'", SJID, "',"
     "'", Nick, "',"
     "'", SSubscription, "',"
     "'", SAsk, "',"
     "'", SAskMessage, "',"
     "'N', '', 'item')"].

group_to_string(#rostergroup{user_host_jid = {User, _Server, {N, D, R} = _JID},
			 grp = Group}, ServerS) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(exmpp_jid:to_list(N, D, R)),
    ["("
      "'", ServerS, "',"
      "'", Username, "',"
      "'", SJID, "',"
      "'", ejabberd_odbc:escape(Group), "')"].
=======
import_terms(LServer, Mods, [Term|Terms]) ->
    import_term(LServer, Mods, Term),
    import_terms(LServer, Mods, Terms);
import_terms(_LServer, _Mods, []) ->
    ok.

import_term(LServer, [{Mod, DBType}|Mods], Term) ->
    case catch Mod:import(LServer, DBType, Term) of
        pass -> import_term(LServer, Mods, Term);
        ok -> ok;
        Err ->
            ?ERROR_MSG("failed to import ~p for module ~p: ~p",
                       [Term, Mod, Err])
    end;
import_term(_LServer, [], _Term) ->
    ok.

prepare_output(FileName) ->
    prepare_output(FileName, normal).

prepare_output(FileName, Type) when is_binary(FileName) ->
    prepare_output(binary_to_list(FileName), Type);
prepare_output(FileName, normal) when is_list(FileName) ->
    case file:open(FileName, [write, raw]) of
        {ok, Fd} ->
            Fd;
        Err ->
            exit(Err)
    end;
prepare_output(FileName, disk_log) when is_list(FileName) ->
    case disk_log:open([{name, make_ref()},
                        {repair, truncate},
			{file, FileName}]) of
        {ok, Fd} ->
            #dump{fd = Fd};
        Err ->
            exit(Err)
    end;
prepare_output(Output, _Type) ->
    Output.

close_output(FileName, Fd) when FileName /= Fd ->
    case Fd of
        #dump{} ->
            disk_log:close(Fd#dump.fd);
        _ ->
            file:close(Fd)
    end,
    ok;
close_output(_, _) ->
    ok.

flatten(SQLs) ->
    flatten(SQLs, []).

flatten([L|Ls], Acc) ->
    flatten(Ls, flatten1(lists:reverse(L), Acc));
flatten([], Acc) ->
    Acc.

flatten1([H|T], Acc) ->
    flatten1(T, [[H, $\n]|Acc]);
flatten1([], Acc) ->
    Acc.
>>>>>>> upstream/master
