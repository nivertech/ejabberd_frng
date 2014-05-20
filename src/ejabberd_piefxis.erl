%%%----------------------------------------------------------------------
%%% File    : ejabberd_piefxis.erl
%%% Author  : Pablo Polvorin, Vidal Santiago Martinez
%%% Purpose : XEP-0227: Portable Import/Export Format for XMPP-IM Servers
%%% Created : 17 Jul 2008 by Pablo Polvorin <pablo.polvorin@process-one.net>
%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2012, Evgeniy Khramtsov
%%% @doc
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

%%% Not implemented:
%%% - Export from mod_offline_odbc.erl
%%% - Export from mod_private_odbc.erl
%%% - XEP-227: 6. Security Considerations
%%% - Other schemas of XInclude are not tested, and may not be imported correctly.
%%% - If a host has many users, split that host in XML files with 50 users each.
%%%% Headers

-module(ejabberd_piefxis).

%% API
-export([import_file/1, export_server/1, export_host/2]).

-define(CHUNK_SIZE, 1024*20). %20k

-include("ejabberd.hrl").
<<<<<<< HEAD
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% Copied from mod_private.erl
-record(private_storage, {user_host_ns, xml}).

=======
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").
-include("mod_roster.hrl").

%%-include_lib("exmpp/include/exmpp.hrl").
%%-include_lib("exmpp/include/exmpp_client.hrl").
%% Copied from exmpp header files:
%% Copied from mod_private.erl
>>>>>>> upstream/master
%%-define(ERROR_MSG(M,Args),io:format(M,Args)).
%%-define(INFO_MSG(M,Args),ok).
%%%==================================
%%%% Import file
-define(NS_PIE, <<"urn:xmpp:pie:0">>).
-define(NS_PIEFXIS, <<"http://www.xmpp.org/extensions/xep-0227.html#ns">>).
-define(NS_XI, <<"http://www.w3.org/2001/XInclude">>).

<<<<<<< HEAD
import_file(FileName) ->
    import_file(FileName, 2).

import_file(FileName, RootDepth) ->
    try_start_exmpp(),
    Dir = filename:dirname(FileName),
    {ok, IO} = try_open_file(FileName),
    Parser = exmpp_xml:start_parser([{max_size,infinity},
				     {root_depth, RootDepth},
				     {emit_endtag,true}]),
    read_chunks(IO, #parsing_state{parser=Parser, dir=Dir}),
    file:close(IO),
    exmpp_xml:stop_parser(Parser).

try_start_exmpp() ->
    try exmpp:start()
    catch
	error:{already_started, exmpp} -> ok;
	  error:undef -> throw({error, exmpp_not_installed})
    end.
=======
-record(state, {xml_stream_state :: xml_stream:xml_stream_state(),
                user = <<"">>    :: binary(),
                server = <<"">>  :: binary(),
                fd               :: file:io_device(),
                dir = <<"">>     :: binary()}).
>>>>>>> upstream/master

-type state() :: #state{}.

%%File could be large.. we read it in chunks
%%%===================================================================
%%% API
%%%===================================================================
import_file(FileName) ->
    import_file(FileName, #state{}).

-spec import_file(binary(), state()) -> ok | {error, atom()}.

import_file(FileName, State) ->
    case file:open(FileName, [read, binary]) of
	{ok, Fd} ->
            Dir = filename:dirname(FileName),
            XMLStreamState = xml_stream:new(self(), infinity),
            Res = process(State#state{xml_stream_state = XMLStreamState,
                                      fd = Fd,
                                      dir = Dir}),
            file:close(Fd),
            Res;
	{error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~s': ~s", [FileName, ErrTxt]),
            {error, Reason}
    end.

%%%==================================
%%%% Process Elements
%%%==================================
%%%% Process Element
<<<<<<< HEAD

process_element(El=#xmlel{name=user, ns=_XMLNS},
		State=#parsing_state{host=Host}) ->
    case add_user(El,Host) of
	ok -> ok;
	{error, _Other} -> error
    end,
    State;

process_element(H=#xmlel{name=host},State) ->
    State#parsing_state{host=exmpp_xml:get_attribute(H,<<"jid">>,none)};

process_element(#xmlel{name='server-data'},State) ->
    State;

process_element(El=#xmlel{name=include, ns=?NS_XINCLUDE}, State=#parsing_state{dir=Dir}) ->
    case exmpp_xml:get_attribute(El, <<"href">>, none) of
	none ->
	    ok;
	HrefB ->
	    Href = binary_to_list(HrefB),
	    %%?INFO_MSG("Parse also this file: ~n~p", [Href]),
	    FileName = filename:join([Dir, Href]),
	    import_file(FileName, 1),
	    Href
    end,
    State;

process_element(#xmlcdata{cdata = _CData},State) ->
    State;

process_element(#xmlendtag{ns = _NS, name='server-data'},State) ->
    State;

process_element(#xmlendtag{ns = _NS, name=_Name},State) ->
    State;

process_element(El,State) ->
    io:format("Warning!: unknown element found: ~p ~n",[El]),
    State.

%%%==================================
%%%% Add user

add_user(El, Domain) ->
    User = exmpp_xml:get_attribute(El,<<"name">>,none),
    PasswordFormat = exmpp_xml:get_attribute(El,<<"password-format">>,<<"plaintext">>),
    Password = exmpp_xml:get_attribute(El,<<"password">>,none),
    add_user(El, Domain, User, PasswordFormat, Password).

%% @spec (El::xmlel(), Domain::string(), User::binary(), PasswordFormat::binary(), Password::binary() | none)
%%       -> ok | {error, ErrorText::string()}
%% @doc Add a new user to the database.
%% If user already exists, it will be only updated.
add_user(El, Domain, User, <<"plaintext">>, none) ->
    io:format("Account ~s@~s will not be created, updating it...~n",
	      [User, Domain]),
    io:format(""),
    populate_user_with_elements(El, Domain, User),
    ok;
add_user(El, Domain, User, <<"scram">> = PasswordFormat, Password) ->
    Password2 = prepare_password(PasswordFormat, Password, El),
    case create_user(User,Password2,Domain) of
	ok ->
	    populate_user_with_elements(El, Domain, User),
	    ok;
	{atomic, exists} ->
	    io:format("Account ~s@~s already exists, updating it...~n",
		      [User, Domain]),
	    io:format(""),
	    populate_user_with_elements(El, Domain, User),
	    ok;
	{error, Other} ->
	    ?ERROR_MSG("Error adding user ~s@~s: ~p~n", [User, Domain, Other]),
	    {error, Other}
    end.
=======
%%%==================================
%%%% Add user
%% @spec (El::xmlel(), Domain::string(), User::binary(), Password::binary() | none)
%%       -> ok | {error, ErrorText::string()}
%% @doc Add a new user to the database.
%% If user already exists, it will be only updated.
-spec export_server(binary()) -> any().
>>>>>>> upstream/master

prepare_password(<<"plaintext">>, PasswordBinary, _El) ->
    ?BTL(PasswordBinary);
prepare_password(<<"scram">>, none, El) ->
    ScramEl = exmpp_xml:get_element(El, 'scram-hash'),
    #scram{storedkey = base64:decode(exmpp_xml:get_attribute(
					ScramEl, <<"stored-key">>, none)),
	   serverkey = base64:decode(exmpp_xml:get_attribute(
					ScramEl, <<"server-key">>, none)),
	   salt = base64:decode(exmpp_xml:get_attribute(
				  ScramEl, <<"salt">>, none)),
	   iterationcount = list_to_integer(exmpp_xml:get_attribute_as_list(
					       ScramEl, <<"iteration-count">>,
					       ?SCRAM_DEFAULT_ITERATION_COUNT))
	  }.

populate_user_with_elements(El, Domain, User) ->
    exmpp_xml:foreach(
      fun (_,Child) ->
	      populate_user(User,Domain,Child)
      end,
      El).

%% @spec (User::string(), Password::string(), Domain::string())
%%       -> ok | {atomic, exists} | {error, not_allowed}
%% @doc  Create a new user
<<<<<<< HEAD
create_user(User,Password,Domain) ->
    case ejabberd_auth:try_register(?BTL(User),?BTL(Domain),Password) of
	{atomic,ok} -> ok;
	{atomic, exists} -> {atomic, exists};
	{error, not_allowed} -> {error, not_allowed};
	Other -> {error, Other}
    end.
=======
export_server(Dir) ->
    export_hosts(?MYHOSTS, Dir).
>>>>>>> upstream/master

%%%==================================
%%%% Populate user
%% @spec (User::string(), Domain::string(), El::xml())
%%      -> ok | {error, not_found}
%%
%% @doc  Add a new user from a XML file with a roster list.
%%
%% Example of a file:
%% ```
%% <?xml version='1.0' encoding='UTF-8'?>
%% <server-data xmlns='http://www.xmpp.org/extensions/xep-0227.html#ns'>
%%   <host jid='localhost'>
%%     <user name='juliet' password='s3crEt'>
%%       <query xmlns='jabber:iq:roster'>
%%         <item jid='romeo@montague.net'
%%               name='Romeo'
%%               subscription='both'>
%%           <group>Friends</group>
%%         </item>
%%       </query>
%%     </user>
%%   </host>
%%  </server-data>
%% '''
-spec export_host(binary(), binary()) -> any().

<<<<<<< HEAD
populate_user(User,Domain,El=#xmlel{name='query', ns='jabber:iq:roster'}) ->
    io:format("Trying to add/update roster list...",[]),
    case loaded_module(Domain,[mod_roster]) of
	{ok, M} ->
	    case M:set_items(User, Domain, El) of
		{atomic, ok} ->
		    io:format(" DONE.~n",[]),
		    ok;
		_ ->
		    io:format(" ERROR.~n",[]),
		    ?ERROR_MSG("Error trying to add a new user: ~s ~n",
			       [exmpp_xml:document_to_list(El)]),
		    {error, not_found}
	    end;
	E -> io:format(" ERROR: ~p~n",[E]),
	     ?ERROR_MSG("Module not loaded: mod_roster ~s ~n",
			[exmpp_xml:document_to_list(El)]),
	     {error, not_found}
    end;

=======
export_host(Dir, Host) ->
    export_hosts([Host], Dir).
>>>>>>> upstream/master

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with vCard tags values
%% @ret  ok | {error, not_found}
%% @doc  Read vcards from the XML and send it to the server
%%
%% Example:
%% ```
%% <?xml version='1.0' encoding='UTF-8'?>
%% <server-data xmlns='http://www.xmpp.org/extensions/xep-0227.html#ns'>
%%   <host jid='localhost'>
%%     <user name='admin' password='s3crEt'>
%%       <vCard xmlns='vcard-temp'>
%%         <FN>Admin</FN>
%%       </vCard>
%%     </user>
%%   </host>
%% </server-data>
%% '''
<<<<<<< HEAD

populate_user(User,Domain,El=#xmlel{name='vCard', ns='vcard-temp'}) ->
    io:format("Trying to add/update vCards...",[]),
    case loaded_module(Domain,[mod_vcard]) of
	{ok, M}  ->  FullUser = exmpp_jid:make(User, Domain),
		     IQ = #iq{kind=request, type = set, payload = El},
		     case M:process_sm_iq(FullUser, FullUser , IQ) of
			 {error,_Err} ->
			     io:format(" ERROR.~n",[]),
			     ?ERROR_MSG("Error processing vcard ~s : ~p ~n",
					[exmpp_xml:document_to_list(El), _Err]);
			 _ ->
			     io:format(" DONE.~n",[]), ok
		     end;
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("Module not loaded: mod_vcard ~s ~n",
		       [exmpp_xml:document_to_list(El)]),
	    {error, not_found}
    end;
=======
%%%===================================================================
%%% Internal functions
%%%===================================================================
export_hosts(Hosts, Dir) ->
    FnT = make_filename_template(),
    DFn = make_main_basefilename(Dir, FnT),
    case file:open(DFn, [raw, write]) of
        {ok, Fd} ->
            print(Fd, make_piefxis_xml_head()),
            print(Fd, make_piefxis_server_head()),
            FilesAndHosts = [{make_host_filename(FnT, Host), Host}
                             || Host <- Hosts],
            lists:foreach(
              fun({FnH, _}) ->
                      print(Fd, make_xinclude(FnH))
              end, FilesAndHosts),
            print(Fd, make_piefxis_server_tail()),
            print(Fd, make_piefxis_xml_tail()),
            file:close(Fd),
            lists:foldl(
              fun({FnH, Host}, ok) ->
                      export_host(Dir, FnH, Host);
                 (_, Err) ->
                      Err
              end, ok, FilesAndHosts);
        {error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~s': ~s", [DFn, ErrTxt]),
            {error, Reason}
    end.
>>>>>>> upstream/master

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with offline messages values
%% @ret  ok | {error, not_found}
%% @doc  Read off-line message from the XML and send it to the server
<<<<<<< HEAD

populate_user(User,Domain,El=#xmlel{name='offline-messages'}) ->
    io:format("Trying to add/update offline-messages...",[]),
    case loaded_module(Domain, [mod_offline]) of
	{ok, M} ->
	    ok = exmpp_xml:foreach(
		   fun (_Element, {xmlcdata, _}) ->
			   ok;
		       (_Element, Child) ->
			   From  = exmpp_xml:get_attribute(Child,<<"from">>,none),
			   FullFrom = exmpp_jid:parse(From),
			   FullUser = exmpp_jid:make(User, Domain),
			   _R = M:store_packet(FullFrom, FullUser, Child)
		   end, El), io:format(" DONE.~n",[]);
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("Module not loaded: mod_offline ~s ~n",
		       [exmpp_xml:document_to_list(El)]),
	    {error, not_found}
    end;
=======
export_host(Dir, FnH, Host) ->
    DFn = make_host_basefilename(Dir, FnH),
    case file:open(DFn, [raw, write]) of
        {ok, Fd} ->
            print(Fd, make_piefxis_xml_head()),
            print(Fd, make_piefxis_host_head(Host)),
            Users = ejabberd_auth:get_vh_registered_users(Host),
            case export_users(Users, Host, Fd) of
                ok ->
                    print(Fd, make_piefxis_host_tail()),
                    print(Fd, make_piefxis_xml_tail()),
                    file:close(Fd),
                    ok;
                Err ->
                    file:close(Fd),
                    file:delete(DFn),
                    Err
            end;
        {error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~s': ~s", [DFn, ErrTxt]),
            {error, Reason}
    end.
>>>>>>> upstream/master

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with private storage values
%% @ret  ok | {error, not_found}
%% @doc  Private storage parsing
<<<<<<< HEAD

populate_user(User,Domain,El=#xmlel{name='query', ns='jabber:iq:private'}) ->
    io:format("Trying to add/update private storage...",[]),
    case loaded_module(Domain,[mod_private]) of
	{ok, M} ->
	    FullUser = exmpp_jid:make(User, Domain),
	    IQ = #iq{type = set,
		     ns = 'jabber:iq:private',
		     kind = request,
		     iq_ns = 'jabberd:client',
		     payload = El},
	    case M:process_sm_iq(FullUser, FullUser, IQ ) of
		{error, _Err} ->
		    io:format(" ERROR.~n",[]),
		    ?ERROR_MSG("Error processing private storage ~s : ~p ~n",
			       [exmpp_xml:document_to_list(El), _Err]);
		_ ->     io:format(" DONE.~n",[]), ok
	    end;
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("Module not loaded: mod_private ~s~n",
		       [exmpp_xml:document_to_list(El)]),
	    {error, not_found}
=======
export_users([{User, _S}|Users], Server, Fd) ->
    case export_user(User, Server, Fd) of
        ok ->
            export_users(Users, Server, Fd);
        Err ->
            Err
>>>>>>> upstream/master
    end;
export_users([], _Server, _Fd) ->
    ok.

%%%==================================
%%%% Utilities
<<<<<<< HEAD

loaded_module(Domain,Options) when is_binary(Domain) ->
    loaded_module(?BTL(Domain),Options);
loaded_module(Domain,Options) ->
    LoadedModules = gen_mod:loaded_modules(Domain),
    case lists:filter(fun(Module) ->
			      lists:member(Module, LoadedModules)
                      end, Options) of
        [M|_] -> {ok, M};
        [] -> {error,not_found}
    end.

%%%==================================

%%%% Export hosts

%% @spec (Dir::string(), Hosts::[string()]) -> ok
export_hosts(Dir, Hosts) ->
    try_start_exmpp(),
=======
export_user(User, Server, Fd) ->
    Pass = ejabberd_auth:get_password_s(User, Server),
    Els = get_offline(User, Server) ++
        get_vcard(User, Server) ++
        get_privacy(User, Server) ++
        get_roster(User, Server) ++
        get_private(User, Server),
    print(Fd, xml:element_to_binary(
                #xmlel{name = <<"user">>,
                       attrs = [{<<"name">>, User},
                                {<<"password">>, Pass}],
                       children = Els})).

get_vcard(User, Server) ->
    JID = jlib:make_jid(User, Server, <<>>),
    case mod_vcard:process_sm_iq(JID, JID, #iq{type = get}) of
        #iq{type = result, sub_el = [_|_] = VCardEls} ->
            VCardEls;
        _ ->
            []
    end.

%%%==================================
get_offline(User, Server) ->
    case mod_offline:get_offline_els(User, Server) of
        [] ->
            [];
        Els ->
            NewEls = lists:map(
                       fun(#xmlel{attrs = Attrs} = El) ->
                               NewAttrs = lists:keystore(<<"xmlns">>, 1,
                                                         Attrs,
                                                         {<<"xmlns">>,
                                                          <<"jabber:client">>}),
                               El#xmlel{attrs = NewAttrs}
                       end, Els),
            [#xmlel{name = <<"offline-messages">>, children = NewEls}]
    end.

%%%% Export hosts
get_privacy(User, Server) ->
    case mod_privacy:get_user_lists(User, Server) of
        {ok, #privacy{default = Default,
                      lists = [_|_] = Lists}} ->
            XLists = lists:map(
                       fun({Name, Items}) ->
                               XItems = lists:map(
                                          fun mod_privacy:item_to_xml/1, Items),
                               #xmlel{name = <<"list">>,
                                      attrs = [{<<"name">>, Name}],
                                      children = XItems}
                       end, Lists),
            DefaultEl = case Default of
                            none ->
                                [];
                            _ ->
                                [#xmlel{name = <<"default">>,
                                        attrs = [{<<"name">>, Default}]}]
                        end,
            [#xmlel{name = <<"query">>,
                    attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
                    children = DefaultEl ++ XLists}];
        _ ->
            []
    end.

%% @spec (Dir::string(), Hosts::[string()]) -> ok
get_roster(User, Server) ->
    JID = jlib:make_jid(User, Server, <<>>),
    case mod_roster:get_roster(User, Server) of
        [_|_] = Items ->
            Subs =
                lists:flatmap(
                  fun(#roster{ask = Ask,
                              askmessage = Msg} = R)
                        when Ask == in; Ask == both ->
                          Status = if is_binary(Msg) -> (Msg);
                                      true -> <<"">>
                                   end,
                          [#xmlel{name = <<"presence">>,
                                  attrs =
                                      [{<<"from">>,
                                        jlib:jid_to_string(R#roster.jid)},
                                       {<<"to">>, jlib:jid_to_string(JID)},
                                       {<<"xmlns">>, <<"jabber:client">>},
                                       {<<"type">>, <<"subscribe">>}],
                                  children =
                                      [#xmlel{name = <<"status">>,
                                              attrs = [],
                                              children =
                                                  [{xmlcdata, Status}]}]}];
                     (_) ->
                          []
                  end, Items),
            Rs = lists:flatmap(
                   fun(#roster{ask = in, subscription = none}) ->
                           [];
                      (R) ->
                           [mod_roster:item_to_xml(R)]
                   end, Items),
            [#xmlel{name = <<"query">>,
                    attrs = [{<<"xmlns">>, ?NS_ROSTER}],
                    children = Rs} | Subs];
        _ ->
            []
    end.

get_private(User, Server) ->
    case mod_private:get_data(User, Server) of
        [_|_] = Els ->
            [#xmlel{name = <<"query">>,
                    attrs = [{<<"xmlns">>, ?NS_PRIVATE}],
                    children = Els}];
        _ ->
            []
    end.

process(#state{xml_stream_state = XMLStreamState, fd = Fd} = State) ->
    case file:read(Fd, ?CHUNK_SIZE) of
        {ok, Data} ->
            NewXMLStreamState = xml_stream:parse(XMLStreamState, Data),
            case process_els(State#state{xml_stream_state =
                                             NewXMLStreamState}) of
                {ok, NewState} ->
                    process(NewState);
                Err ->
                    xml_stream:close(NewXMLStreamState),
                    Err
            end;
        eof ->
            xml_stream:close(XMLStreamState),
            ok
    end.
>>>>>>> upstream/master

process_els(State) ->
    receive
        {'$gen_event', El} ->
            case process_el(El, State) of
                {ok, NewState} ->
                    process_els(NewState);
                Err ->
                    Err
            end
    after 0 ->
            {ok, State}
    end.

process_el({xmlstreamstart, <<"server-data">>, Attrs}, State) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_PIEFXIS ->
            {ok, State};
        ?NS_PIE ->
            {ok, State};
        NS ->
            stop("Unknown 'server-data' namespace = ~s", [NS])
    end;
process_el({xmlstreamend, _}, State) ->
    {ok, State};
process_el({xmlstreamcdata, _}, State) ->
    {ok, State};
process_el({xmlstreamelement, #xmlel{name = <<"xi:include">>,
                                     attrs = Attrs}},
           #state{dir = Dir, user = <<"">>} = State) ->
    FileName = xml:get_attr_s(<<"href">>, Attrs),
    case import_file(filename:join([Dir, FileName]), State) of
        ok ->
            {ok, State};
        Err ->
            Err
    end;
process_el({xmlstreamstart, <<"host">>, Attrs}, State) ->
    process_el({xmlstreamelement, #xmlel{name = <<"host">>,
                                         attrs = Attrs}}, State);
process_el({xmlstreamelement, #xmlel{name = <<"host">>,
                                     attrs = Attrs,
                                     children = Els}}, State) ->
    JIDS = xml:get_attr_s(<<"jid">>, Attrs),
    case jlib:string_to_jid(JIDS) of
        #jid{lserver = S} ->
            case lists:member(S, ?MYHOSTS) of
                true ->
                    process_users(Els, State#state{server = S});
                false ->
                    stop("Unknown host: ~s", [S])
            end;
        error ->
            stop("Invalid 'jid': ~s", [JIDS])
    end;
process_el({xmlstreamstart, <<"user">>, Attrs}, State = #state{server = S})
  when S /= <<"">> ->
    process_el({xmlstreamelement, #xmlel{name = <<"user">>, attrs = Attrs}},
               State);
process_el({xmlstreamelement, #xmlel{name = <<"user">>} = El},
           State = #state{server = S}) when S /= <<"">> ->
    process_user(El, State);
process_el({xmlstreamelement, El}, State = #state{server = S, user = U})
  when S /= <<"">>, U /= <<"">> ->
    process_user_el(El, State);
process_el({xmlstreamelement, El}, _State) ->
    stop("Unexpected tag: ~p", [El]);
process_el({xmlstreamstart, El, Attrs}, _State) ->
    stop("Unexpected payload: ~p", [{El, Attrs}]);
process_el({xmlstreamerror, Err}, _State) ->
    stop("Failed to process element = ~p", [Err]).

process_users([#xmlel{} = El|Els], State) ->
    case process_user(El, State) of
        {ok, NewState} ->
            process_users(Els, NewState);
        Err ->
            Err
    end;
process_users([_|Els], State) ->
    process_users(Els, State);
process_users([], State) ->
    {ok, State}.

process_user(#xmlel{name = <<"user">>, attrs = Attrs, children = Els},
             #state{server = LServer} = State) ->
    Name = xml:get_attr_s(<<"name">>, Attrs),
    Pass = xml:get_attr_s(<<"password">>, Attrs),
    case jlib:nodeprep(Name) of
        error ->
            stop("Invalid 'user': ~s", [Name]);
        LUser ->
            case ejabberd_auth:try_register(LUser, LServer, Pass) of
                {atomic, _} ->
                    process_user_els(Els, State#state{user = LUser});
                Err ->
                    stop("Failed to create user '~s': ~p", [Name, Err])
            end
    end.

<<<<<<< HEAD
    FilesAndHosts = [{make_host_filename(FnT, Host), Host} || Host <- Hosts],
    [print(Fd, make_xinclude(FnH)) || {FnH, _Host} <- FilesAndHosts],
=======
process_user_els([#xmlel{} = El|Els], State) ->
    case process_user_el(El, State) of
        {ok, NewState} ->
            process_user_els(Els, NewState);
        Err ->
            Err
    end;
process_user_els([_|Els], State) ->
    process_user_els(Els, State);
process_user_els([], State) ->
    {ok, State}.

process_user_el(#xmlel{name = Name, attrs = Attrs, children = Els} = El,
                State) ->
    case {Name, xml:get_attr_s(<<"xmlns">>, Attrs)} of
        {<<"query">>, ?NS_ROSTER} ->
            process_roster(El, State);
        {<<"query">>, ?NS_PRIVACY} ->
            %% Make sure <list/> elements go before <active/> and <default/>
            NewEls = lists:reverse(lists:keysort(#xmlel.name, Els)),
            process_privacy_el(El#xmlel{children = NewEls}, State);
        {<<"query">>, ?NS_PRIVATE} ->
            process_private(El, State);
        {<<"vCard">>, ?NS_VCARD} ->
            process_vcard(El, State);
        {<<"offline-messages">>, _} ->
            process_offline_msgs(Els, State);
        {<<"presence">>, <<"jabber:client">>} ->
            process_presence(El, State);
        _ ->
            {ok, State}
    end.
>>>>>>> upstream/master

process_privacy_el(#xmlel{children = [#xmlel{} = SubEl|SubEls]} = El, State) ->
    case process_privacy(#xmlel{children = [SubEl]}, State) of
        {ok, NewState} ->
            process_privacy_el(El#xmlel{children = SubEls}, NewState);
        Err ->
            Err
    end;
process_privacy_el(#xmlel{children = [_|SubEls]} = El, State) ->
    process_privacy_el(El#xmlel{children = SubEls}, State);
process_privacy_el(#xmlel{children = []}, State) ->
    {ok, State}.

process_offline_msgs([#xmlel{} = El|Els], State) ->
    case process_offline_msg(El, State) of
        {ok, NewState} ->
            process_offline_msgs(Els, NewState);
        Err ->
            Err
    end;
process_offline_msgs([_|Els], State) ->
    process_offline_msgs(Els, State);
process_offline_msgs([], State) ->
    {ok, State}.

process_roster(El, State = #state{user = U, server = S}) ->
    case mod_roster:set_items(U, S, El) of
        {atomic, _} ->
            {ok, State};
        Err ->
            stop("Failed to write roster: ~p", [Err])
    end.

%%%==================================
%%%% Export server
process_privacy(El, State = #state{user = U, server = S}) ->
    JID = jlib:make_jid(U, S, <<"">>),
    case mod_privacy:process_iq_set(
           [], JID, JID, #iq{type = set, sub_el = El}) of
        {error, _} = Err ->
            stop("Failed to write privacy: ~p", [Err]);
        _ ->
            {ok, State}
    end.

%% @spec (Dir::string()) -> ok
process_private(El, State = #state{user = U, server = S}) ->
    JID = jlib:make_jid(U, S, <<"">>),
    case mod_private:process_sm_iq(
           JID, JID, #iq{type = set, sub_el = El}) of
        #iq{type = result} ->
            {ok, State};
        Err ->
            stop("Failed to write private: ~p", [Err])
    end.

%%%==================================
%%%% Export server

%% @spec (Dir::string()) -> ok
export_server(Dir) ->
    Hosts = ?MYHOSTS,
    export_hosts(Dir, Hosts).

%%%==================================
%%%% Export host
process_vcard(El, State = #state{user = U, server = S}) ->
    JID = jlib:make_jid(U, S, <<"">>),
    case mod_vcard:process_sm_iq(
           JID, JID, #iq{type = set, sub_el = El}) of
        #iq{type = result} ->
            {ok, State};
        Err ->
            stop("Failed to write vcard: ~p", [Err])
    end.

%% @spec (Dir::string(), Host::string()) -> ok
<<<<<<< HEAD
export_host(Dir, Host) ->
    Hosts = [Host],
    export_hosts(Dir, Hosts).
=======
process_offline_msg(El, State = #state{user = U, server = S}) ->
    FromS = xml:get_attr_s(<<"from">>, El#xmlel.attrs),
    case jlib:string_to_jid(FromS) of
        #jid{} = From ->
            To = jlib:make_jid(U, S, <<>>),
            NewEl = jlib:replace_from_to(From, To, El),
            case catch mod_offline:store_packet(From, To, NewEl) of
                {'EXIT', _} = Err ->
                    stop("Failed to store offline message: ~p", [Err]);
                _ ->
                    {ok, State}
            end;
        _ ->
            stop("Invalid 'from' = ~s", [FromS])
    end.
>>>>>>> upstream/master

%% @spec (Dir::string(), Fn::string(), Host::string()) -> ok
process_presence(El, #state{user = U, server = S} = State) ->
    FromS = xml:get_attr_s(<<"from">>, El#xmlel.attrs),
    case jlib:string_to_jid(FromS) of
        #jid{} = From ->
            To = jlib:make_jid(U, S, <<>>),
            NewEl = jlib:replace_from_to(From, To, El),
            ejabberd_router:route(From, To, NewEl),
            {ok, State};
        _ ->
            stop("Invalid 'from' = ~s", [FromS])
    end.

stop(Fmt, Args) ->
    ?ERROR_MSG(Fmt, Args),
    {error, import_failed}.

make_filename_template() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    list_to_binary(
      io_lib:format("~4..0w~2..0w~2..0w-~2..0w~2..0w~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).

<<<<<<< HEAD
    Users = ejabberd_auth:get_vh_registered_users(Host),
    [export_user(Fd, Username, Host) || {Username, _Host} <- Users],
    timer:sleep(500), % Delay to ensure ERROR_MSG are displayed in the shell
=======
make_main_basefilename(Dir, FnT) ->
    Filename2 = <<FnT/binary, ".xml">>,
    filename:join([Dir, Filename2]).
>>>>>>> upstream/master

%% @doc Make the filename for the host.
%% Example: ``(<<"20080804-231550">>, <<"jabber.example.org">>) ->
%%             <<"20080804-231550_jabber_example_org.xml">>''
make_host_filename(FnT, Host) ->
    Host2 = str:join(str:tokens(Host, <<".">>), <<"_">>),
    <<FnT/binary, "_", Host2/binary, ".xml">>.

%%%==================================
%%%% PIEFXIS formatting
make_host_basefilename(Dir, FnT) ->
    filename:join([Dir, FnT]).

%% @spec () -> string()
make_piefxis_xml_head() ->
    "<?xml version='1.0' encoding='UTF-8'?>".

%% @spec () -> string()
make_piefxis_xml_tail() ->
    "".

%% @spec () -> string()
make_piefxis_server_head() ->
    io_lib:format("<server-data xmlns='~s' xmlns:xi='~s'>",
                  [?NS_PIE, ?NS_XI]).

%% @spec () -> string()
make_piefxis_server_tail() ->
    "</server-data>".

%% @spec (Host::string()) -> string()
make_piefxis_host_head(Host) ->
    io_lib:format("<host xmlns='~s' xmlns:xi='~s' jid='~s'>",
                  [?NS_PIE, ?NS_XI, Host]).

%% @spec () -> string()
make_piefxis_host_tail() ->
    "</host>".

%% @spec (Fn::string()) -> string()
make_xinclude(Fn) ->
    Base = filename:basename(Fn),
    io_lib:format("<xi:include href='~s'/>", [Base]).

%%%==================================
%%%% Export user
%% @spec (Fd, Username::string(), Host::string()) -> ok
%% @doc Extract user information and print it.
<<<<<<< HEAD
export_user(Fd, Username, Host) ->
    try extract_user(Username, Host) of
	UserString when is_list(UserString) ->
	    print(Fd, UserString)
    catch
	E1:E2 ->
	    ?ERROR_MSG("The account ~s@~s is not exported because a problem "
		       "was found in it:~n~p: ~p", [Username, Host, E1, E2])
    end.

%% @spec (Username::string(), Host::string()) -> string()
extract_user(Username, Host) ->
    Password = ejabberd_auth:get_password(Username, Host),
    PasswordStr = build_password_string(Password),
    UserInfo = [extract_user_info(InfoName, Username, Host) || InfoName <- [roster, offline, private, vcard]],
    UserInfoString = lists:flatten(UserInfo),
    io_lib:format("<user name='~s' ~s ~s</user>",
		  [Username, PasswordStr, UserInfoString]).

build_password_string({StoredKey, ServerKey, Salt, IterationCount}) ->
    io_lib:format("password-format='scram'>"
		  "<scram-hash stored-key='~s' server-key='~s' "
		  "salt='~s' iteration-count='~w'/> ",
		  [base64:encode_to_string(StoredKey),
		   base64:encode_to_string(ServerKey),
		   base64:encode_to_string(Salt),
		   IterationCount]);
build_password_string(Password) when is_list(Password) ->
    io_lib:format("password-format='plaintext' password='~s'>", [Password]).

%% @spec (InfoName::atom(), Username::string(), Host::string()) -> string()
extract_user_info(roster, Username, Host) ->
    case loaded_module(Host,[mod_roster]) of
	{ok, M} ->
	    From = To = exmpp_jid:make(Username, Host, ""),
	    SubelGet = exmpp_xml:element(?NS_ROSTER, 'query', [], []),
	    IQGet = #iq{kind=request, type=get, ns=?NS_ROSTER, payload=SubelGet},
	    Res = M:process_local_iq(From, To, IQGet),
	    case Res#iq.payload of
		undefined -> "";
		El -> exmpp_xml:document_to_list(El)
	    end;
	_E ->
	    ""
    end;

extract_user_info(offline, Username, Host) ->
    case loaded_module(Host,[mod_offline]) of
	{ok, mod_offline} ->
	    Els = mnesia_pop_offline_messages([], Username, Host),
	    case Els of
		[] -> "";
		Els ->
                    OfEl = #xmlel{name = 'offline-messages',
                                  children = Els},
		    %OfEl = {xmlelement, "offline-messages", [], Els},
		    exmpp_xml:document_to_list(OfEl)
	    end;
	_E ->
	    ""
    end;

extract_user_info(private, Username, Host) ->
    case loaded_module(Host,[mod_private]) of
	{ok, mod_private} ->
	    get_user_private_mnesia(Username, Host);
	_E ->
	    ""
    end;

extract_user_info(vcard, Username, Host) ->
    case loaded_module(Host,[mod_vcard]) of
	{ok, M} ->
	    From = To = exmpp_jid:make(Username, Host, ""),
	    SubelGet = exmpp_xml:element(?NS_VCARD, 'vCard', [], []),
	    IQGet = #iq{kind=request, type=get, ns=?NS_VCARD, payload=SubelGet},
	    Res = M:process_sm_iq(From, To, IQGet),
	    case Res#iq.payload of
		undefined -> "";
		El -> exmpp_xml:document_to_list(El)
	    end;
	_E ->
	    ""
    end.

=======
%% @spec (Username::string(), Host::string()) -> string()
%% @spec (InfoName::atom(), Username::string(), Host::string()) -> string()
>>>>>>> upstream/master
%%%==================================
%%%% Interface with ejabberd offline storage
%% Copied from mod_offline.erl and customized
<<<<<<< HEAD
-record(offline_msg, {user_host, timestamp, expire, from, to, packet}).
mnesia_pop_offline_messages(Ls, User, Server) ->
    try
	LUser = User,
	LServer = Server,
	US = {LUser, LServer},
	F = fun() ->
		    Rs = mnesia:wread({offline_msg, US}),
		    %% mnesia:delete({offline_msg, US}),
		    Rs
	    end,
	case mnesia:transaction(F) of
	    {atomic, Rs} ->
		TS = make_timestamp(),
		Ls ++ lists:map(
			fun(R) ->
				[Packet] = exmpp_xml:parse_document(R#offline_msg.packet, [names_as_atom]),
				FromString = exmpp_jid:prep_to_list(R#offline_msg.from),
				Packet2 = exmpp_xml:set_attribute(Packet, <<"from">>, FromString),
				Packet3 = Packet2#xmlel{ns = ?NS_JABBER_CLIENT},
				exmpp_xml:append_children(
				  Packet3,
				  [jlib:timestamp_to_xml(
				     calendar:gregorian_seconds_to_datetime(
				       R#offline_msg.timestamp),
				     utc,
				     exmpp_jid:make("", Server, ""),
				     "Offline Storage"),
				   %% TODO: Delete the next three lines once XEP-0091 is Obsolete
				   jlib:timestamp_to_xml(
				     calendar:gregorian_seconds_to_datetime(
				       R#offline_msg.timestamp))]
				 )
			end,
			lists:filter(
			  fun(R) ->
				  case R#offline_msg.expire of
				      0 ->
					  true;
				      TimeStamp ->
					  TS < TimeStamp
				  end
			  end,
			  lists:keysort(#offline_msg.timestamp, Rs)));
	    _ ->
		Ls
	end
    catch
	_ ->
	    Ls
    end.

make_timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.

%%%==================================
%%%% Interface with ejabberd private storage

get_user_private_mnesia(Username, Host) ->
    ListNsEl = mnesia:dirty_select(private_storage,
				   [{#private_storage{user_host_ns={?LTB(Username), ?LTB(Host), '$1'}, xml = '$2'},
				     [], ['$$']}]),
    Els = [lists:flatten(exmpp_xml:document_to_list(El)) || [_Ns, El] <- ListNsEl],
    case lists:flatten(Els) of
	"" -> "";
	ElsString ->
	    io_lib:format("<query xmlns='jabber:iq:private'>~s</query>", [ElsString])
    end.

=======
%%%==================================
%%%% Interface with ejabberd private storage
>>>>>>> upstream/master
%%%==================================
%%%% Disk file access
%% @spec () -> string()
%% @spec (Dir::string(), FnT::string()) -> string()
%% @spec (FnT::string(), Host::string()) -> FnH::string()
%% @doc Make the filename for the host.
%% Example: ``("20080804-231550", "jabber.example.org") -> "20080804-231550_jabber_example_org.xml"''
%% @spec (Fn::string()) -> {ok, Fd}
%% @spec (Fd) -> ok
%% @spec (Fd, String::string()) -> ok
print(Fd, String) ->
%%%==================================
%%% vim: set filetype=erlang tabstop=8 foldmarker=%%%%,%%%= foldmethod=marker:
    file:write(Fd, String).
