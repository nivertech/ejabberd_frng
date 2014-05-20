%%%----------------------------------------------------------------------
%%% File    : ejabberd_config.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Load config file
%%% Created : 14 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_config).
-author('alexey@process-one.net').

<<<<<<< HEAD
-export([start/0, load_file/1, get_host_option/2,
	 add_global_option/2, add_local_option/2,
         mne_add_local_option/2, mne_del_local_option/1,
	 del_global_option/1, del_local_option/1,
	 get_global_option/1, get_local_option/1]).

-export([for_host/1
         ,configure_host/2
         ,delete_host/1
        ]).

-export([search/1]).

-export([get_vh_by_auth_method/1]).
-export([is_file_readable/1]).
=======
-export([start/0, load_file/1, read_file/1,
	 add_global_option/2, add_local_option/2,
	 get_global_option/2, get_local_option/2,
         get_global_option/3, get_local_option/3,
         get_option/2, get_option/3, add_option/2,
         get_vh_by_auth_method/1, is_file_readable/1,
         get_version/0, get_myhosts/0, get_mylang/0,
         prepare_opt_val/4, convert_table_to_binary/5,
         transform_options/1, collect_options/1,
         convert_to_yaml/1, convert_to_yaml/2]).
>>>>>>> upstream/master

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_config.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {opts = [],
		hosts = [],
		override_local = false,
		override_global = false,
		override_acls = false}).

%% @type macro() = {macro_key(), macro_value()}

%% @type macro_key() = atom().
%% The atom must have all characters in uppercase.

%% @type macro_value() = term().


start() ->
    case catch mnesia:table_info(local_config, storage_type) of
        disc_copies ->
            mnesia:delete_table(local_config);
        _ ->
            ok
    end,
    mnesia:create_table(local_config,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
    Config = get_ejabberd_config_path(),
    State = read_file(Config),
    %% This start time is used by mod_last:
    {MegaSecs, Secs, _} = now(),
    UnixTime = MegaSecs*1000000 + Secs,
    State1 = set_option({node_start, global}, UnixTime, State),
    set_opts(State1).

%% @doc Get the filename of the ejabberd configuration file.
%% The filename can be specified with: erl -config "/path/to/ejabberd.yml".
%% It can also be specified with the environtment variable EJABBERD_CONFIG_PATH.
%% If not specified, the default value 'ejabberd.yml' is assumed.
%% @spec () -> string()
get_ejabberd_config_path() ->
    case application:get_env(config) of
	{ok, Path} -> Path;
	undefined ->
	    case os:getenv("EJABBERD_CONFIG_PATH") of
		false ->
		    ?CONFIG_PATH;
		Path ->
		    Path
	    end
    end.

%% @doc Read the ejabberd configuration file.
%% It also includes additional configuration files and replaces macros.
%% This function will crash if finds some error in the configuration file.
%% @spec (File::string()) -> #state{}.
read_file(File) ->
    read_file(File, [{replace_macros, true},
                     {include_files, true}]).

read_file(File, Opts) ->
    Terms1 = get_plain_terms_file(File, Opts),
    Terms_macros = case proplists:get_bool(replace_macros, Opts) of
                       true -> replace_macros(Terms1);
                       false -> Terms1
                   end,
    Terms = transform_terms(Terms_macros),
    State = lists:foldl(fun search_hosts/2, #state{}, Terms),
    {Head, Tail} = lists:partition(
                     fun({host_config, _}) -> false;
                        ({append_host_config, _}) -> false;
                        (_) -> true
                     end, Terms),
    State1 = lists:foldl(fun process_term/2, State, Head ++ Tail),
    State1#state{opts = compact(State1#state.opts)}.

-spec load_file(string()) -> ok.

load_file(File) ->
    State = read_file(File),
    set_opts(State).

-spec convert_to_yaml(file:filename()) -> ok | {error, any()}.

convert_to_yaml(File) ->
    convert_to_yaml(File, stdout).

-spec convert_to_yaml(file:filename(),
                      stdout | file:filename()) -> ok | {error, any()}.

convert_to_yaml(File, Output) ->
    State = read_file(File, [{include_files, false}]),
    Opts = [{K, V} || #local_config{key = K, value = V} <- State#state.opts],
    {GOpts, HOpts} = split_by_hosts(Opts),
    NewOpts = GOpts ++ lists:map(
                         fun({Host, Opts1}) ->
                                 {host_config, [{Host, Opts1}]}
                         end, HOpts),
    Data = p1_yaml:encode(lists:reverse(NewOpts)),
    case Output of
        stdout ->
            io:format("~s~n", [Data]);
        FileName ->
            file:write_file(FileName, Data)
    end.

%% @doc Read an ejabberd configuration file and return the terms.
%% Input is an absolute or relative path to an ejabberd config file.
%% Returns a list of plain terms,
%% in which the options 'include_config_file' were parsed
%% and the terms in those files were included.
%% @spec(string()) -> [term()]
%% @spec(iolist()) -> [term()]
get_plain_terms_file(File) ->
    get_plain_terms_file(File, [{include_files, true}]).

get_plain_terms_file(File, Opts) when is_binary(File) ->
    get_plain_terms_file(binary_to_list(File), Opts);
get_plain_terms_file(File1, Opts) ->
    File = get_absolute_path(File1),
    case consult(File) of
	{ok, Terms} ->
<<<<<<< HEAD
	    include_config_files(Terms);
	{error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
	    ExitText = describe_config_problem(File, Reason, LineNumber),
	    ?ERROR_MSG(ExitText, []),
	    exit_or_halt(ExitText);
	{error, Reason} ->
	    ExitText = describe_config_problem(File, Reason),
	    ?ERROR_MSG(ExitText, []),
	    exit_or_halt(ExitText)
=======
            BinTerms = strings_to_binary(Terms),
            case proplists:get_bool(include_files, Opts) of
                true ->
                    include_config_files(BinTerms);
                false ->
                    BinTerms
            end;
	{error, Reason} ->
	    ?ERROR_MSG(Reason, []),
	    exit_or_halt(Reason)
    end.

consult(File) ->
    case filename:extension(File) of
        ".yml" ->
            case p1_yaml:decode_from_file(File, [plain_as_atom]) of
                {ok, []} ->
                    {ok, []};
                {ok, [Document|_]} ->
                    {ok, Document};
                {error, Err} ->
                    {error, p1_yaml:format_error(Err)}
            end;
        _ ->
            case file:consult(File) of
                {ok, Terms} ->
                    {ok, Terms};
                {error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
                    {error, describe_config_problem(File, Reason, LineNumber)};
                {error, Reason} ->
                    {error, describe_config_problem(File, Reason)}
            end
>>>>>>> upstream/master
    end.

%% @doc Convert configuration filename to absolute path.
%% Input is an absolute or relative path to an ejabberd configuration file.
%% And returns an absolute path to the configuration file.
%% @spec (string()) -> string()
get_absolute_path(File) ->
    case filename:pathtype(File) of
	absolute ->
	    File;
	relative ->
	    Config_path = get_ejabberd_config_path(),
	    Config_dir = filename:dirname(Config_path),
	    filename:absname_join(Config_dir, File)
    end.


search_hosts(Term, State) ->
    case Term of
	{host, Host} ->
	    if
		State#state.hosts == [] ->
		    add_hosts_to_option([Host], State);
		true ->
		    ?ERROR_MSG("Can't load config file: "
			       "too many hosts definitions", []),
		    exit("too many hosts definitions")
	    end;
	{hosts, Hosts} ->
	    if
		State#state.hosts == [] ->
		    add_hosts_to_option(Hosts, State);
		true ->
		    ?ERROR_MSG("Can't load config file: "
			       "too many hosts definitions", []),
		    exit("too many hosts definitions")
	    end;
	_ ->
	    State
    end.

add_hosts_to_option(Hosts, State) ->
<<<<<<< HEAD
    PrepHosts1 = normalize_hosts(Hosts),
    PrepHosts = ensure_localhost_is_first(PrepHosts1),
    mnesia:transaction(
      fun() ->
	      lists:foreach(
		fun(H) ->
			mnesia:write(#local_config{key = {H, host},
						   value = []})
		end, PrepHosts)
      end),
    add_option(hosts, PrepHosts, State#state{hosts = PrepHosts}).
=======
    PrepHosts = normalize_hosts(Hosts),
    set_option({hosts, global}, PrepHosts, State#state{hosts = PrepHosts}).
>>>>>>> upstream/master

normalize_hosts(Hosts) ->
    normalize_hosts(Hosts,[]).
normalize_hosts([], PrepHosts) ->
    lists:reverse(PrepHosts);
normalize_hosts([Host|Hosts], PrepHosts) ->
<<<<<<< HEAD
    try
	PrepHost = exmpp_stringprep:nodeprep(Host),
	normalize_hosts(Hosts, [PrepHost|PrepHosts])
    catch
	_ ->
=======
    case jlib:nodeprep(iolist_to_binary(Host)) of
	error ->
>>>>>>> upstream/master
	    ?ERROR_MSG("Can't load config file: "
		       "invalid host name [~p]", [Host]),
	    exit("invalid hostname")
    end.

%% @spec (Hosts::[string()]) -> [Localhost::string() | [string()]]
%% @doc Return the list where the first is surely "localhost".
ensure_localhost_is_first(Hosts) ->
    case lists:all(fun is_list/1, Hosts) of
	true ->
	    ensure_localhost_is_first1(Hosts);
	false -> 
	    ?ERROR_MSG("This list of hosts is bad formed:~n~p", [Hosts]),
	    ensure_localhost_is_first1([])
    end.

ensure_localhost_is_first1(["localhost" | _] = Hosts) ->
    Hosts;
ensure_localhost_is_first1(Hosts) ->
    case lists:member("localhost", Hosts) of
	true ->
	    ["localhost" | lists:delete("localhost", Hosts)];
	false ->
	    ?INFO_MSG("ejabberd added the default virtual host \"localhost\""
		      " to the list of hosts.", []),
	    ["localhost" | Hosts]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

describe_config_problem(Filename, Reason) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" : " ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    ExitText.

describe_config_problem(Filename, Reason, LineNumber) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" approximately in the line "
			  ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    Lines = get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR_MSG("The following lines from your configuration file might be"
	       " relevant to the error: ~n~s", [Lines]),
    ExitText.

get_config_lines(Filename, TargetNumber, PreContext, PostContext) ->
    {ok, Fd} = file:open(Filename, [read]),
    LNumbers = lists:seq(TargetNumber-PreContext, TargetNumber+PostContext),
    NextL = io:get_line(Fd, no_prompt),
    R = get_config_lines2(Fd, NextL, 1, LNumbers, []),
    file:close(Fd),
    R.

get_config_lines2(_Fd, eof, _CurrLine, _LNumbers, R) ->
    lists:reverse(R);
get_config_lines2(_Fd, _NewLine, _CurrLine, [], R) ->
    lists:reverse(R);
get_config_lines2(Fd, Data, CurrLine, [NextWanted | LNumbers], R) when is_list(Data) ->
    NextL = io:get_line(Fd, no_prompt),
    if
	CurrLine >= NextWanted ->
	    Line2 = [integer_to_list(CurrLine), ": " | Data],
	    get_config_lines2(Fd, NextL, CurrLine+1, LNumbers, [Line2 | R]);
	true ->
	    get_config_lines2(Fd, NextL, CurrLine+1, [NextWanted | LNumbers], R)
    end.

%% If ejabberd isn't yet running in this node, then halt the node
exit_or_halt(ExitText) ->
    case [Vsn || {ejabberd, _Desc, Vsn} <- application:which_applications()] of
	[] ->
	    timer:sleep(1000),
	    halt(string:substr(ExitText, 1, 199));
	[_] ->
	    exit(ExitText)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

describe_config_problem(Filename, Reason) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" : " ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    ExitText.

describe_config_problem(Filename, Reason, LineNumber) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" approximately in the line "
			  ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    Lines = get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR_MSG("The following lines from your configuration file might be"
	       " relevant to the error: ~n~s", [Lines]),
    ExitText.

get_config_lines(Filename, TargetNumber, PreContext, PostContext) ->
    {ok, Fd} = file:open(Filename, [read]),
    LNumbers = lists:seq(TargetNumber-PreContext, TargetNumber+PostContext),
    NextL = io:get_line(Fd, no_prompt),
    R = get_config_lines2(Fd, NextL, 1, LNumbers, []),
    file:close(Fd),
    R.

get_config_lines2(_Fd, eof, _CurrLine, _LNumbers, R) ->
    lists:reverse(R);
get_config_lines2(_Fd, _NewLine, _CurrLine, [], R) ->
    lists:reverse(R);
get_config_lines2(Fd, Data, CurrLine, [NextWanted | LNumbers], R) when is_list(Data) ->
    NextL = io:get_line(Fd, no_prompt),
    if
	CurrLine >= NextWanted ->
	    Line2 = [integer_to_list(CurrLine), ": " | Data],
	    get_config_lines2(Fd, NextL, CurrLine+1, LNumbers, [Line2 | R]);
	true ->
	    get_config_lines2(Fd, NextL, CurrLine+1, [NextWanted | LNumbers], R)
    end.

%% If ejabberd isn't yet running in this node, then halt the node
exit_or_halt(ExitText) ->
    case [Vsn || {ejabberd, _Desc, Vsn} <- application:which_applications()] of
	[] ->
	    timer:sleep(1000),
	    halt(string:substr(ExitText, 1, 199));
	[_] ->
	    exit(ExitText)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for 'include_config_file'

%% @doc Include additional configuration files in the list of terms.
%% @spec ([term()]) -> [term()]
include_config_files(Terms) ->
    {FileOpts, Terms1} =
        lists:mapfoldl(
          fun({include_config_file, _} = T, Ts) ->
                  {[transform_include_option(T)], Ts};
             ({include_config_file, _, _} = T, Ts) ->
                  {[transform_include_option(T)], Ts};
             (T, Ts) ->
                  {[], [T|Ts]}
          end, [], Terms),
    Terms2 = lists:flatmap(
               fun({File, Opts}) ->
                       include_config_file(File, Opts)
               end, lists:flatten(FileOpts)),
    Terms1 ++ Terms2.

transform_include_option({include_config_file, File}) when is_list(File) ->
    case is_string(File) of
        true -> {File, []};
        false -> File
    end;
transform_include_option({include_config_file, Filename}) ->
    {Filename, []};
transform_include_option({include_config_file, Filename, Options}) ->
    {Filename, Options}.

include_config_file(Filename, Options) ->
    Included_terms = get_plain_terms_file(Filename),
    Disallow = proplists:get_value(disallow, Options, []),
    Included_terms2 = delete_disallowed(Disallow, Included_terms),
    Allow_only = proplists:get_value(allow_only, Options, all),
    keep_only_allowed(Allow_only, Included_terms2).

%% @doc Filter from the list of terms the disallowed.
%% Returns a sublist of Terms without the ones which first element is
%% included in Disallowed.
%% @spec (Disallowed::[atom()], Terms::[term()]) -> [term()]
delete_disallowed(Disallowed, Terms) ->
    lists:foldl(
      fun(Dis, Ldis) ->
	      delete_disallowed2(Dis, Ldis)
      end,
      Terms,
      Disallowed).

delete_disallowed2(Disallowed, [H|T]) ->
    case element(1, H) of
	Disallowed ->
	    ?WARNING_MSG("The option '~p' is disallowed, "
			 "and will not be accepted", [Disallowed]),
	    delete_disallowed2(Disallowed, T);
	_ ->
	    [H|delete_disallowed2(Disallowed, T)]
    end;
delete_disallowed2(_, []) ->
    [].

%% @doc Keep from the list only the allowed terms.
%% Returns a sublist of Terms with only the ones which first element is
%% included in Allowed.
%% @spec (Allowed::[atom()], Terms::[term()]) -> [term()]
keep_only_allowed(all, Terms) ->
    Terms;
keep_only_allowed(Allowed, Terms) ->
    {As, NAs} = lists:partition(
		  fun(Term) ->
			  lists:member(element(1, Term), Allowed)
		  end,
		  Terms),
    [?WARNING_MSG("This option is not allowed, "
		  "and will not be accepted:~n~p", [NA])
     || NA <- NAs],
    As.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for Macro

%% @doc Replace the macros with their defined values.
%% @spec (Terms::[term()]) -> [term()]
replace_macros(Terms) ->
    {TermsOthers, Macros} = split_terms_macros(Terms),
    replace(TermsOthers, Macros).

%% @doc Split Terms into normal terms and macro definitions.
%% @spec (Terms) -> {Terms, Macros}
%%       Terms = [term()]
%%       Macros = [macro()]
split_terms_macros(Terms) ->
    lists:foldl(
      fun(Term, {TOs, Ms}) ->
	      case Term of
		  {define_macro, Key, Value} -> 
		      case is_correct_macro({Key, Value}) of
			  true -> 
			      {TOs, Ms++[{Key, Value}]};
			  false -> 
			      exit({macro_not_properly_defined, Term})
		      end;
                  {define_macro, KeyVals} ->
                      case lists:all(fun is_correct_macro/1, KeyVals) of
                          true ->
                              {TOs, Ms ++ KeyVals};
                          false ->
                              exit({macros_not_properly_defined, Term})
                      end;
		  Term ->
		      {TOs ++ [Term], Ms}
	      end
      end,
      {[], []},
      Terms).

is_correct_macro({Key, _Val}) ->
    is_atom(Key) and is_all_uppercase(Key);
is_correct_macro(_) ->
    false.

%% @doc Recursively replace in Terms macro usages with the defined value.
%% @spec (Terms, Macros) -> Terms
%%       Terms = [term()]
%%       Macros = [macro()]
replace([], _) ->
    [];
replace([Term|Terms], Macros) ->
    [replace_term(Term, Macros) | replace(Terms, Macros)];
replace(Term, Macros) ->
    replace_term(Term, Macros).

replace_term(Key, Macros) when is_atom(Key) ->
    case is_all_uppercase(Key) of
	true ->
	    case proplists:get_value(Key, Macros) of
		undefined -> exit({undefined_macro, Key});
		Value -> Value
	    end;
	false ->
	    Key
    end;
replace_term({use_macro, Key, Value}, Macros) ->
    proplists:get_value(Key, Macros, Value);
replace_term(Term, Macros) when is_list(Term) ->
    replace(Term, Macros);
replace_term(Term, Macros) when is_tuple(Term) ->
    List = tuple_to_list(Term),
    List2 = replace(List, Macros),
    list_to_tuple(List2);
replace_term(Term, _) ->
    Term.

is_all_uppercase(Atom) ->
    String = erlang:atom_to_list(Atom),
    lists:all(fun(C) when C >= $a, C =< $z -> false;
		 (_) -> true
	      end, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process terms

process_term(Term, State) ->
    case Term of
<<<<<<< HEAD
	override_global ->
	    State#state{override_global = true};
	override_local ->
	    State#state{override_local = true};
	override_acls ->
	    State#state{override_acls = true};
	{acl, _ACLName, _ACLData} ->
	    process_host_term(Term, global, State);
	{access, _RuleName, _Rules} ->
	    process_host_term(Term, global, State);
	{shaper, _Name, _Data} ->
	    %%lists:foldl(fun(Host, S) -> process_host_term(Term, Host, S) end,
	    %%    	State, State#state.hosts);
	    process_host_term(Term, global, State);
	{host, _Host} ->
	    State;
	{hosts, _Hosts} ->
	    State;
	{host_config, Host, Terms} ->
	    lists:foldl(fun(T, S) -> process_host_term(T, Host, S) end,
			State, Terms);
	{clusterid, ClusterID} ->
	    add_option(clusterid, ClusterID, State);
	{listen, Listeners} ->
	    Listeners2 =
		lists:map(
		  fun({PortIP, Module, Opts}) ->
			  {Port, IPT, _, _, Proto, OptsClean} =
			      ejabberd_listener:parse_listener_portip(PortIP, Opts),
			  {{Port, IPT, Proto}, Module, OptsClean}
		  end,
		  Listeners),
	    add_option(listen, Listeners2, State);
	{language, Val} ->
	    add_option(language, Val, State);
	{outgoing_s2s_port, Port} ->
	    add_option(outgoing_s2s_port, Port, State);
	{outgoing_s2s_options, Methods, Timeout} ->
	    add_option(outgoing_s2s_options, {Methods, Timeout}, State);
 	{outgoing_s2s_local_address, Addr} ->
 	    add_option(outgoing_s2s_local_address, Addr, State);
        {s2s_dns_options, PropList} ->
            add_option(s2s_dns_options, PropList, State);
	{s2s_use_starttls, Port} ->
	    add_option(s2s_use_starttls, Port, State);
	{s2s_certfile, CertFile} ->
	    case ejabberd_config:is_file_readable(CertFile) of
		true -> add_option(s2s_certfile, CertFile, State);
		false ->
		    ErrorText = "There is a problem in the configuration: "
			"the specified file is not readable: ",
		    throw({error, ErrorText ++ CertFile})
	    end;
	{domain_certfile, Domain, CertFile} ->
	    case ejabberd_config:is_file_readable(CertFile) of
		true -> add_option({domain_certfile, Domain}, CertFile, State);
		false ->
		    ErrorText = "There is a problem in the configuration: "
			"the specified file is not readable: ",
		    throw({error, ErrorText ++ CertFile})
	    end;
	{node_type, NodeType} ->
	    add_option(node_type, NodeType, State);
	{cluster_nodes, Nodes} ->
	    add_option(cluster_nodes, Nodes, State);
	{domain_balancing, Domain, Balancing} ->
	    add_option({domain_balancing, Domain}, Balancing, State);
	{domain_balancing_component_number, Domain, N} ->
	    add_option({domain_balancing_component_number, Domain}, N, State);
	{watchdog_admins, Admins} ->
	    add_option(watchdog_admins, Admins, State);
	{watchdog_large_heap, LH} ->
	    add_option(watchdog_large_heap, LH, State);
	{registration_timeout, Timeout} ->
	    add_option(registration_timeout, Timeout, State);
	{ejabberdctl_access_commands, ACs} ->
	    add_option(ejabberdctl_access_commands, ACs, State);
	{captcha_cmd, Cmd} ->
	    add_option(captcha_cmd, Cmd, State);
	{captcha_host, Host} ->
	    add_option(captcha_host, Host, State);
	{loglevel, Loglevel} ->
	    ejabberd_loglevel:set(Loglevel),
	    State;
	{max_fsm_queue, N} ->
	    add_option(max_fsm_queue, N, State);
	{_Opt, _Val} ->
	    process_host_term(Term, global, State)
=======
	{host_config, HostTerms} ->
            lists:foldl(
              fun({Host, Terms}, AccState) ->
                      lists:foldl(fun(T, S) ->
                                          process_host_term(T, Host, S, set)
                                  end, AccState, Terms)
              end, State, HostTerms);
        {append_host_config, HostTerms} ->
            lists:foldl(
              fun({Host, Terms}, AccState) ->
                      lists:foldl(fun(T, S) ->
                                          process_host_term(T, Host, S, append)
                                  end, AccState, Terms)
              end, State, HostTerms);
	_ ->
            process_host_term(Term, global, State, set)
>>>>>>> upstream/master
    end.

process_host_term(Term, Host, State, Action) ->
    case Term of
<<<<<<< HEAD
	{acl, ACLName, ACLData} ->
	    State#state{opts =
			[acl:to_record(Host, ACLName, ACLData) | State#state.opts]};
	{access, RuleName, Rules} ->
	    State#state{opts = [#config{key = {access, RuleName, Host},
					value = Rules} |
				State#state.opts]};
	{shaper, Name, Data} ->
	    State#state{opts = [#config{key = {shaper, Name, Host},
					value = Data} |
				State#state.opts]};
	{host, Host} ->
	    State;
	{hosts, _Hosts} ->
	    State;
	{odbc_server, ODBC_server} ->
	    add_option({odbc_server, Host}, ODBC_server, State);
	{auth_method, Methods} ->
	    {Methods2, StorageOption} = replace_storage_auth(Host, Methods),
	    State2 = case StorageOption of
		{auth_storage, Storage} ->
		    add_option({auth_storage, Host}, Storage, State);
		undefined -> 
		    State
	    end,
	    add_option({auth_method, Host}, Methods2, State2);
	{Opt, Val} ->
	    add_option({Opt, Host}, Val, State)
    end.

add_option(Opt, Val, State) ->
    Table = case Opt of
		hosts ->
		    config;
		language ->
		    config;
		_ ->
		    local_config
	    end,
    case Table of
	config ->
	    State#state{opts = [#config{key = Opt, value = Val} |
				State#state.opts]};
	local_config ->
	    case Opt of
		{{add, OptName}, Host} ->
		    State#state{opts = compact({OptName, Host}, Val,
					       State#state.opts, [])};
		_ ->
		    State#state{opts = [#local_config{key = Opt, value = Val} |
					State#state.opts]}
	    end
    end.

compact({OptName, Host} = Opt, Val, [], Os) ->
    ?WARNING_MSG("The option '~p' is defined for the host ~p using host_config "
    "before the global '~p' option. This host_config option may get overwritten.", [OptName, Host, OptName]),
    [#local_config{key = Opt, value = Val}] ++ Os;
%% Traverse the list of the options already parsed
compact(Opt, Val, [O | Os1], Os2) ->
    case catch O#local_config.key of
	%% If the key of a local_config matches the Opt that wants to be added
	Opt ->
	    %% Then prepend the new value to the list of old values
	    Os2 ++ [#local_config{key = Opt,
				  value = Val++O#local_config.value}
		   ] ++ Os1;
	_ ->
	    compact(Opt, Val, Os1, Os2++[O])
=======
        {modules, Modules} when Action == set ->
            set_option({modules, Host}, replace_modules(Modules), State);
        {modules, Modules} when Action == append ->
            append_option({modules, Host}, replace_modules(Modules), State);
        {host, _} ->
            State;
        {hosts, _} ->
            State;
	{Opt, Val} when Action == set ->
	    set_option({Opt, Host}, Val, State);
        {Opt, Val} when Action == append ->
            append_option({Opt, Host}, Val, State);
        Opt ->
            ?WARNING_MSG("Ignore invalid (outdated?) option ~p", [Opt]),
            State
>>>>>>> upstream/master
    end.

set_option(Opt, Val, State) ->
    State#state{opts = [#local_config{key = Opt, value = Val} |
                        State#state.opts]}.

append_option({Opt, Host}, Val, State) ->
    GlobalVals = lists:flatmap(
                   fun(#local_config{key = {O, global}, value = V})
                         when O == Opt ->
                           if is_list(V) -> V;
                              true -> [V]
                           end;
                      (_) ->
                           []
                   end, State#state.opts),
    NewVal = if is_list(Val) -> Val ++ GlobalVals;
                true -> [Val|GlobalVals]
             end,
    set_option({Opt, Host}, NewVal, State).

set_opts(State) ->
    Opts = State#state.opts,
    F = fun() ->
		lists:foreach(fun(R) ->
				      mnesia:write(R)
			      end, Opts)
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted,{no_exists,Table}} ->
	    MnesiaDirectory = mnesia:system_info(directory),
	    ?ERROR_MSG("Error reading Mnesia database spool files:~n"
		       "The Mnesia database couldn't read the spool file for the table '~p'.~n"
		       "ejabberd needs read and write access in the directory:~n   ~s~n"
		       "Maybe the problem is a change in the computer hostname,~n"
		       "or a change in the Erlang node name, which is currently:~n   ~p~n"
		       "Check the ejabberd guide for details about changing the~n"
		       "computer hostname or Erlang node name.~n",
		       [Table, MnesiaDirectory, node()]),
	    exit("Error reading Mnesia database")
    end.

add_global_option(Opt, Val) ->
    add_option(Opt, Val).

add_local_option(Opt, Val) ->
<<<<<<< HEAD
    mnesia:transaction(fun mne_add_local_option/2, [Opt, Val]).

mne_add_local_option(Opt, Val) ->
    mnesia:write(#local_config{key = Opt,
                               value = Val}).

del_global_option(Opt) ->
    mnesia:transaction(fun() ->
			       mnesia:delete({config, Opt})
		       end).

del_local_option(Opt) ->
=======
    add_option(Opt, Val).

add_option(Opt, Val) when is_atom(Opt) ->
    add_option({Opt, global}, Val);
add_option(Opt, Val) ->
>>>>>>> upstream/master
    mnesia:transaction(fun() ->
			       mnesia:delete({local_config, Opt})
		       end).

<<<<<<< HEAD

get_global_option({Opt1, Host} = Opt) when is_list(Host) ->
    case ets:lookup(config, Opt) of
	[#config{value = Val}] ->
	    Val;
	_ ->
	    get_global_option({Opt1, global})
    end;
get_global_option(Opt) ->
    case ets:lookup(config, Opt) of
	[#config{value = Val}] when Opt == hosts ->
	    ensure_localhost_is_first(Val);
	[#config{value = Val}] ->
	    Val;
	_ ->
	    undefined
    end.

get_local_option({Opt1, Host} = Opt) when is_list(Host) ->
    case ets:lookup(local_config, Opt) of
	[#local_config{value = Val}] ->
	    Val;
	_ ->
	    get_local_option({Opt1, global})
    end;
get_local_option(Opt) ->
=======
-spec prepare_opt_val(any(), any(), check_fun(), any()) -> any().

prepare_opt_val(Opt, Val, F, Default) ->
    Res = case F of
              {Mod, Fun} ->
                  catch Mod:Fun(Val);
              _ ->
                  catch F(Val)
          end,
    case Res of
        {'EXIT', _} ->
            ?INFO_MSG("Configuration problem:~n"
                      "** Option: ~s~n"
                      "** Invalid value: ~s~n"
                      "** Using as fallback: ~s",
                      [format_term(Opt),
                       format_term(Val),
                       format_term(Default)]),
            Default;
        _ ->
            Res
    end.

-type check_fun() :: fun((any()) -> any()) | {module(), atom()}.

-spec get_global_option(any(), check_fun()) -> any().

get_global_option(Opt, F) ->
    get_option(Opt, F, undefined).

-spec get_global_option(any(), check_fun(), any()) -> any().

get_global_option(Opt, F, Default) ->
    get_option(Opt, F, Default).

-spec get_local_option(any(), check_fun()) -> any().

get_local_option(Opt, F) ->
    get_option(Opt, F, undefined).

-spec get_local_option(any(), check_fun(), any()) -> any().

get_local_option(Opt, F, Default) ->
    get_option(Opt, F, Default).

-spec get_option(any(), check_fun()) -> any().

get_option(Opt, F) ->
    get_option(Opt, F, undefined).

-spec get_option(any(), check_fun(), any()) -> any().

get_option(Opt, F, Default) when is_atom(Opt) ->
    get_option({Opt, global}, F, Default);
get_option(Opt, F, Default) ->
    case Opt of
        {O, global} when is_atom(O) -> ok;
        {O, H} when is_atom(O), is_binary(H) -> ok;
        _ -> ?WARNING_MSG("Option ~p has invalid (outdated?) format. "
                          "This is likely a bug", [Opt])
    end,
>>>>>>> upstream/master
    case ets:lookup(local_config, Opt) of
	[#local_config{value = Val}] ->
	    prepare_opt_val(Opt, Val, F, Default);
        _ ->
            case Opt of
                {Key, Host} when Host /= global ->
                    get_option({Key, global}, F, Default);
                _ ->
                    Default
            end
    end.

<<<<<<< HEAD
get_host_option(Host, Option) ->
    case ets:lookup(local_config, {Option, Host}) of
        [#local_config{value=V}] -> V;
        _ -> undefined
    end.

mne_del_local_option({_OptName, Host} = Opt) when is_list(Host) ->
    mnesia:delete({local_config, Opt});
mne_del_local_option({Host, host} = Opt) when is_list(Host) ->
    mnesia:delete({local_config, Opt}).
=======
-spec get_vh_by_auth_method(atom()) -> [binary()].
>>>>>>> upstream/master

%% Return the list of hosts handled by a given module
get_vh_by_auth_method(AuthMethod) ->
    mnesia:dirty_select(local_config,
			[{#local_config{key = {auth_method, '$1'},
					value=AuthMethod},[],['$1']}]).

%% @spec (Path::string()) -> true | false
is_file_readable(Path) ->
    case file:read_file_info(Path) of
	{ok, FileInfo} ->
	    case {FileInfo#file_info.type, FileInfo#file_info.access} of
		{regular, read} -> true;
		{regular, read_write} -> true;
		_ -> false
	    end;
	{error, _Reason} ->
	    false
    end.

<<<<<<< HEAD
search(Pattern) ->
    {atomic, Res} = mnesia:transaction(fun mnesia:select/2, [local_config, Pattern]),
    Res.

for_host(Host) ->
    mnesia:read({local_config, {Host, host}})
        ++ mnesia:select(local_config,
                  ets:fun2ms(fun (#local_config{key={_, H}})
                                 when H =:= Host ->
                                     object()
                             end))
        ++ acl:for_host(Host).

delete_host(Host) ->
    mnesia_delete_objects(for_host(Host)),
    ok.

configure_host(Host, Config) ->
    HostExistenceTerm = {{Host, host}, []},
    Records = host_terms_to_records(Host, [HostExistenceTerm | Config]),
    mnesia_write_objects(Records),
    ok.

host_terms_to_records(Host, Terms) ->
    lists:foldl(fun (Term, Acc) ->
                        host_term_to_record(Term, Host, Acc)
                end, [], Terms).

host_term_to_record({acl, ACLName, ACLData}, Host, Acc) ->
    [acl:to_record(Host, ACLName, ACLData) | Acc];
host_term_to_record({access, RuleName, Rules}, Host, Acc) ->
    [#config{key={access, RuleName, Host}, value=Rules} | Acc];
host_term_to_record({shaper, Name, Data}, Host, Acc) ->
    [#config{key={shaper, Name, Host}, value=Data} | Acc];
host_term_to_record({host, _}, _Host, Acc) -> Acc;
host_term_to_record({hosts, _}, _Host, Acc) -> Acc;
host_term_to_record({{Host, host}, []}, Host, Acc) ->
    [#local_config{key={Host, host}, value=[]} | Acc];
host_term_to_record({Opt, Val}, Host, Acc) when is_atom(Opt) ->
    [#local_config{key={Opt, Host}, value=Val} | Acc].

    
mnesia_delete_objects(List) when is_list(List) ->
    true = lists:all(fun (I) ->
                             ok =:= mnesia:delete_object(I)
                     end, List).
mnesia_write_objects(List) when is_list(List) ->
    true = lists:all(fun (I) ->
                             ok =:= mnesia:write(I)
                     end, List).

%% Replace internal and odbc auth_methods with storage.
%% Only one storage type can be used, either internal or odbc.
replace_storage_auth(Host, Val) when not is_list(Val) ->
    replace_storage_auth(Host, [Val]);
replace_storage_auth(Host, Val) ->
    replace_storage_auth(Host, Val, [], undefined).

replace_storage_auth(_Host, [], Val2, Storage) ->
    {lists:reverse(Val2), Storage};

replace_storage_auth(Host, [internal = Val | ValT], Val2, undefined) ->
    Storage = {auth_storage, mnesia},
    ?WARNING_MSG("The auth method '~p' is deprecated.~nReplace it with 'storage'"
		 " and also add this option: ~n~p.", [Val, Storage]),
    replace_storage_auth(Host, ValT, [storage | Val2], Storage);

replace_storage_auth(Host, [odbc = Val | ValT], Val2, undefined) ->
    Storage = {auth_storage, odbc},
    ?WARNING_MSG("The auth method '~p' is deprecated.~nReplace it with 'storage'"
		 " and also add this option: ~n~p.", [Val, Storage]),
    replace_storage_auth(Host, ValT, [storage | Val2], Storage);

replace_storage_auth(Host, [Val | ValT], Val2, Storage)
  when (Val /= internal) and (Val /= odbc) ->
    replace_storage_auth(Host, ValT, [Val | Val2], Storage);

replace_storage_auth(Host, [Val | _ValT], _Val2, Storage) ->
    ?CRITICAL_MSG("The auth method '~p' conflicts with~n~p in the host \"~p\"."
		  "~nOnly one of them can be used in each host.",
		  [Val, Storage, Host]),
    throw({unacceptable_auth_conflict, Host, Val, Storage}).
=======
get_version() ->
    list_to_binary(element(2, application:get_key(ejabberd, vsn))).

-spec get_myhosts() -> [binary()].

get_myhosts() ->
    get_option(hosts, fun(V) -> V end).

-spec get_mylang() -> binary().

get_mylang() ->
    get_option(
      language,
      fun iolist_to_binary/1,
      <<"en">>).

replace_module(mod_announce_odbc) -> {mod_announce, odbc};
replace_module(mod_blocking_odbc) -> {mod_blocking, odbc};
replace_module(mod_caps_odbc) -> {mod_caps, odbc};
replace_module(mod_irc_odbc) -> {mod_irc, odbc};
replace_module(mod_last_odbc) -> {mod_last, odbc};
replace_module(mod_muc_odbc) -> {mod_muc, odbc};
replace_module(mod_offline_odbc) -> {mod_offline, odbc};
replace_module(mod_privacy_odbc) -> {mod_privacy, odbc};
replace_module(mod_private_odbc) -> {mod_private, odbc};
replace_module(mod_roster_odbc) -> {mod_roster, odbc};
replace_module(mod_shared_roster_odbc) -> {mod_shared_roster, odbc};
replace_module(mod_vcard_odbc) -> {mod_vcard, odbc};
replace_module(mod_vcard_xupdate_odbc) -> {mod_vcard_xupdate, odbc};
replace_module(Module) -> Module.

replace_modules(Modules) ->
    lists:map(
      fun({Module, Opts}) ->
              case replace_module(Module) of
                  {NewModule, DBType} ->
                      emit_deprecation_warning(Module, NewModule, DBType),
                      NewOpts = [{db_type, DBType} |
                                 lists:keydelete(db_type, 1, Opts)],
                      {NewModule, transform_module_options(Module, NewOpts)};
                  NewModule ->
                      if Module /= NewModule ->
                              emit_deprecation_warning(Module, NewModule);
                         true ->
                              ok
                      end,
                      {NewModule, transform_module_options(Module, Opts)}
              end
      end, Modules).

strings_to_binary([]) ->
    [];
strings_to_binary(L) when is_list(L) ->
    case is_string(L) of
        true ->
            list_to_binary(L);
        false ->
            strings_to_binary1(L)
    end;
strings_to_binary({A, B, C, D}) when
	is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    {A, B, C ,D};
strings_to_binary(T) when is_tuple(T) ->
    list_to_tuple(strings_to_binary1(tuple_to_list(T)));
strings_to_binary(X) ->
    X.

strings_to_binary1([El|L]) ->
    [strings_to_binary(El)|strings_to_binary1(L)];
strings_to_binary1([]) ->
    [];
strings_to_binary1(T) ->
    T.

is_string([C|T]) when (C >= 0) and (C =< 255) ->
    is_string(T);
is_string([]) ->
    true;
is_string(_) ->
    false.

binary_to_strings(B) when is_binary(B) ->
    binary_to_list(B);
binary_to_strings([H|T]) ->
    [binary_to_strings(H)|binary_to_strings(T)];
binary_to_strings(T) when is_tuple(T) ->
    list_to_tuple(binary_to_strings(tuple_to_list(T)));
binary_to_strings(T) ->
    T.

format_term(Bin) when is_binary(Bin) ->
    io_lib:format("\"~s\"", [Bin]);
format_term(S) when is_list(S), S /= [] ->
    case lists:all(fun(C) -> (C>=0) and (C=<255) end, S) of
        true ->
            io_lib:format("\"~s\"", [S]);
        false ->
            io_lib:format("~p", [binary_to_strings(S)])
    end;
format_term(T) ->
    io_lib:format("~p", [binary_to_strings(T)]).

transform_terms(Terms) ->
    %% We could check all ejabberd beams, but this
    %% slows down start-up procedure :(
    Mods = [mod_register,
            mod_last,
            ejabberd_s2s,
            ejabberd_listener,
            ejabberd_odbc_sup,
            shaper,
            ejabberd_s2s_out,
            acl,
            ejabberd_config],
    collect_options(transform_terms(Mods, Terms)).

transform_terms([Mod|Mods], Terms) ->
    case catch Mod:transform_options(Terms) of
        {'EXIT', _} = Err ->
            ?ERROR_MSG("Failed to transform terms by ~p: ~p", [Mod, Err]),
            transform_terms(Mods, Terms);
        NewTerms ->
            transform_terms(Mods, NewTerms)
    end;
transform_terms([], NewTerms) ->
    NewTerms.

transform_module_options(Module, Opts) ->
    Opts1 = gen_iq_handler:transform_module_options(Opts),
    try
        Module:transform_module_options(Opts1)
    catch error:undef ->
            Opts1
    end.

compact(Cfg) ->
    Opts = [{K, V} || #local_config{key = K, value = V} <- Cfg],
    {GOpts, HOpts} = split_by_hosts(Opts),
    [#local_config{key = {O, global}, value = V} || {O, V} <- GOpts] ++
        lists:flatmap(
          fun({Host, OptVal}) ->
                  case lists:member(OptVal, GOpts) of
                      true ->
                          [];
                      false ->
                          [#local_config{key = {Opt, Host}, value = Val}
                           || {Opt, Val} <- OptVal]
                  end
          end, lists:flatten(HOpts)).

split_by_hosts(Opts) ->
    Opts1 = orddict:to_list(
              lists:foldl(
                fun({{Opt, Host}, Val}, D) ->
                        orddict:append(Host, {Opt, Val}, D)
                end, orddict:new(), Opts)),
    case lists:keytake(global, 1, Opts1) of
        {value, {global, GlobalOpts}, HostOpts} ->
            {GlobalOpts, HostOpts};
        _ ->
            {[], Opts1}
    end.

collect_options(Opts) ->
    {D, InvalidOpts} =
        lists:foldl(
          fun({K, V}, {D, Os}) when is_list(V) ->
                  {orddict:append_list(K, V, D), Os};
             ({K, V}, {D, Os}) ->
                  {orddict:store(K, V, D), Os};
             (Opt, {D, Os}) ->
                  {D, [Opt|Os]}
          end, {orddict:new(), []}, Opts),
    InvalidOpts ++ orddict:to_list(D).

transform_options(Opts) ->
    Opts1 = lists:foldl(fun transform_options/2, [], Opts),
    {HOpts, Opts2} = lists:mapfoldl(
                       fun({host_config, O}, Os) ->
                               {[O], Os};
                          (O, Os) ->
                               {[], [O|Os]}
                       end, [], Opts1),
    {AHOpts, Opts3} = lists:mapfoldl(
                        fun({append_host_config, O}, Os) ->
                                {[O], Os};
                           (O, Os) ->
                                {[], [O|Os]}
                        end, [], Opts2),
    HOpts1 = case collect_options(lists:flatten(HOpts)) of
                 [] ->
                     [];
                 HOs ->
                     [{host_config,
                       [{H, transform_terms(O)} || {H, O} <- HOs]}]
             end,
    AHOpts1 = case collect_options(lists:flatten(AHOpts)) of
                  [] ->
                      [];
                  AHOs ->
                      [{append_host_config,
                        [{H, transform_terms(O)} || {H, O} <- AHOs]}]
              end,
    HOpts1 ++ AHOpts1 ++ Opts3.

transform_options({domain_certfile, Domain, CertFile}, Opts) ->
    ?WARNING_MSG("Option 'domain_certfile' now should be defined "
                 "per virtual host or globally. The old format is "
                 "still supported but it is better to fix your config", []),
    [{host_config, [{Domain, [{domain_certfile, CertFile}]}]}|Opts];
transform_options(Opt, Opts) when Opt == override_global;
                                  Opt == override_local;
                                  Opt == override_acls ->
    ?WARNING_MSG("Ignoring '~s' option which has no effect anymore", [Opt]),
    Opts;
transform_options({host_config, Host, HOpts}, Opts) ->
    {AddOpts, HOpts1} =
        lists:mapfoldl(
          fun({{add, Opt}, Val}, Os) ->
                  ?WARNING_MSG("Option 'add' is deprecated. "
                               "The option is still supported "
                               "but it is better to fix your config: "
                               "use 'append_host_config' instead.", []),
                  {[{Opt, Val}], Os};
             (O, Os) ->
                  {[], [O|Os]}
          end, [], HOpts),
    [{append_host_config, [{Host, lists:flatten(AddOpts)}]},
     {host_config, [{Host, HOpts1}]}|Opts];
transform_options({define_macro, Macro, Val}, Opts) ->
    [{define_macro, [{Macro, Val}]}|Opts];
transform_options({include_config_file, _} = Opt, Opts) ->
    [{include_config_file, [transform_include_option(Opt)]} | Opts];
transform_options({include_config_file, _, _} = Opt, Opts) ->
    [{include_config_file, [transform_include_option(Opt)]} | Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

-spec convert_table_to_binary(atom(), [atom()], atom(),
                              fun(), fun()) -> ok.

convert_table_to_binary(Tab, Fields, Type, DetectFun, ConvertFun) ->
    case is_table_still_list(Tab, DetectFun) of
        true ->
            ?INFO_MSG("Converting '~s' table from strings to binaries.", [Tab]),
            TmpTab = list_to_atom(atom_to_list(Tab) ++ "_tmp_table"),
            catch mnesia:delete_table(TmpTab),
            case mnesia:create_table(TmpTab,
                                     [{disc_only_copies, [node()]},
                                      {type, Type},
                                      {local_content, true},
                                      {record_name, Tab},
                                      {attributes, Fields}]) of
                {atomic, ok} ->
                    mnesia:transform_table(Tab, ignore, Fields),
                    case mnesia:transaction(
                           fun() ->
                                   mnesia:write_lock_table(TmpTab),
                                   mnesia:foldl(
                                     fun(R, _) ->
                                             NewR = ConvertFun(R),
                                             mnesia:dirty_write(TmpTab, NewR)
                                     end, ok, Tab)
                           end) of
                        {atomic, ok} ->
                            mnesia:clear_table(Tab),
                            case mnesia:transaction(
                                   fun() ->
                                           mnesia:write_lock_table(Tab),
                                           mnesia:foldl(
                                             fun(R, _) ->
                                                     mnesia:dirty_write(R)
                                             end, ok, TmpTab)
                                   end) of
                                {atomic, ok} ->
                                    mnesia:delete_table(TmpTab);
                                Err ->
                                    report_and_stop(Tab, Err)
                            end;
                        Err ->
                            report_and_stop(Tab, Err)
                    end;
                Err ->
                    report_and_stop(Tab, Err)
            end;
        false ->
            ok
    end.

is_table_still_list(Tab, DetectFun) ->
    is_table_still_list(Tab, DetectFun, mnesia:dirty_first(Tab)).

is_table_still_list(_Tab, _DetectFun, '$end_of_table') ->
    false;
is_table_still_list(Tab, DetectFun, Key) ->
    Rs = mnesia:dirty_read(Tab, Key),
    Res = lists:foldl(fun(_, true) ->
                              true;
                         (_, false) ->
                              false;
                         (R, _) ->
                              case DetectFun(R) of
                                  '$next' ->
                                      '$next';
                                  El ->
                                      is_list(El)
                              end
                      end, '$next', Rs),
    case Res of
        true ->
            true;
        false ->
            false;
        '$next' ->
            is_table_still_list(Tab, DetectFun, mnesia:dirty_next(Tab, Key))
    end.

report_and_stop(Tab, Err) ->
    ErrTxt = lists:flatten(
               io_lib:format(
                 "Failed to convert '~s' table to binary: ~p",
                 [Tab, Err])),
    ?CRITICAL_MSG(ErrTxt, []),
    timer:sleep(1000),
    halt(string:substr(ErrTxt, 1, 199)).

emit_deprecation_warning(Module, NewModule, DBType) ->
    ?WARNING_MSG("Module ~s is deprecated, use {~s, [{db_type, ~s}, ...]}"
                 " instead", [Module, NewModule, DBType]).

emit_deprecation_warning(Module, NewModule) ->
    ?WARNING_MSG("Module ~s is deprecated, use ~s instead",
                 [Module, NewModule]).
>>>>>>> upstream/master
