%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_register).

-author('alexey@process-one.net').

-behaviour(gen_mod).

<<<<<<< HEAD
-export([start/2,
	 stop/1,
	 stream_feature_register/2,
	 unauthenticated_iq_register/4,
	 try_register/5,
	 process_iq/3]).
=======
-export([start/2, stop/1, stream_feature_register/2,
	 unauthenticated_iq_register/4, try_register/5,
	 process_iq/3, send_registration_notifications/3,
         transform_options/1, transform_module_options/1]).
>>>>>>> upstream/master

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
<<<<<<< HEAD

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_INBAND_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_INBAND_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, HostB,
 		       ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, HostB,
 		       ?MODULE, unauthenticated_iq_register, 50),
=======
-include("logger.hrl").

-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_REGISTER, ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_REGISTER, ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, Host, ?MODULE,
		       stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, Host,
		       ?MODULE, unauthenticated_iq_register, 50),
>>>>>>> upstream/master
    mnesia:create_table(mod_register_ip,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_ip, node(),
			  ram_copies),
    ok.

stop(Host) ->
<<<<<<< HEAD
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(c2s_stream_features, HostB,
 			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, HostB,
			  ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_INBAND_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_INBAND_REGISTER).
=======
    ejabberd_hooks:delete(c2s_stream_features, Host,
			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,
			  ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_REGISTER).
>>>>>>> upstream/master

stream_feature_register(Acc, _Host) ->
    [#xmlel{name = <<"register">>,
	    attrs = [{<<"xmlns">>, ?NS_FEATURE_IQREGISTER}],
	    children = []}
     | Acc].

<<<<<<< HEAD
stream_feature_register(Acc, _Host) ->
    [#xmlel{ns = ?NS_INBAND_REGISTER_FEAT, name = 'register'} | Acc].

unauthenticated_iq_register(_Acc,
			    Server, #iq{ns = ?NS_INBAND_REGISTER} = IQ_Rec, IP) ->
=======
unauthenticated_iq_register(_Acc, Server,
			    #iq{xmlns = ?NS_REGISTER} = IQ, IP) ->
>>>>>>> upstream/master
    Address = case IP of
		{A, _Port} -> A;
		_ -> undefined
	      end,
<<<<<<< HEAD
    BareJID = exmpp_jid:make(Server),
    ResIQ = process_iq(exmpp_jid:make(),
 		       BareJID,
 		       IQ_Rec,
		       Address),
    exmpp_iq:iq_to_xmlel(ResIQ, BareJID, exmpp_jid:make());

=======
    ResIQ = process_iq(jlib:make_jid(<<"">>, <<"">>,
				     <<"">>),
		       jlib:make_jid(<<"">>, Server, <<"">>), IQ, Address),
    Res1 = jlib:replace_from_to(jlib:make_jid(<<"">>,
					      Server, <<"">>),
				jlib:make_jid(<<"">>, <<"">>, <<"">>),
				jlib:iq_to_xml(ResIQ)),
    jlib:remove_attr(<<"to">>, Res1);
>>>>>>> upstream/master
unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
    Acc.

process_iq(From, To, IQ) ->
<<<<<<< HEAD
    process_iq(From, To, IQ, {exmpp_jid:prep_node_as_list(From),
                              exmpp_jid:prep_domain_as_list(From),
                              exmpp_jid:prep_resource_as_list(From)}).

process_iq(From, To,
	   #iq{type = Type, lang = Lang, payload = SubEl, id = ID} = IQ_Rec,
	   Source) ->
    IsCaptchaEnabled = case gen_mod:get_module_opt(
			      exmpp_jid:domain_as_list(To), ?MODULE, captcha_protected, false) of
			   true ->
			       true;
			   _ ->
			       false
		       end,
    case Type of
	set ->
	    UTag = exmpp_xml:get_element(SubEl, 'username'),
	    PTag = exmpp_xml:get_element(SubEl, 'password'),
	    RTag = exmpp_xml:get_element(SubEl, 'remove'),
	    Server = exmpp_jid:prep_domain_as_list(To),
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	    AllowRemove = (allow == acl:match_rule(Server, Access, From)),
	    if
		(UTag /= undefined) and (RTag /= undefined) and AllowRemove ->
		    User = exmpp_xml:get_cdata_as_list(UTag),
		    case {exmpp_jid:node_as_list(From), exmpp_jid:prep_domain_as_list(From)} of
		    {User, Server} ->
			    ejabberd_auth:remove_user(User, Server),
                            exmpp_iq:result(IQ_Rec, SubEl);
			_ ->
			    if
				PTag /= undefined ->
				    Password = exmpp_xml:get_cdata_as_list(PTag),
				    case ejabberd_auth:remove_user(User,
								   Server,
								   Password) of
					ok ->
                                            exmpp_iq:result(IQ_Rec, SubEl);
					%% TODO FIXME: This piece of
					%% code does not work since
					%% the code have been changed
					%% to allow several auth
					%% modules.  lists:foreach can
					%% only return ok:
					not_allowed ->
                                            exmpp_iq:error(IQ_Rec,
                                              'not-allowed');
					not_exists ->
                                            exmpp_iq:error(IQ_Rec,
                                              'item-not-found');
					_ ->
                                            exmpp_iq:error(IQ_Rec,
                                              'internal-server-error')
				    end;
				true ->
                                    exmpp_iq:error(IQ_Rec, 'bad-request')
			    end
		    end;
		(UTag == undefined) and (RTag /= undefined) and AllowRemove ->
		    case {exmpp_jid:node_as_list(From),
                  exmpp_jid:prep_domain_as_list(From),
                  exmpp_jid:resource_as_list(From)}of
			{User, Server, Resource} ->
			    ResIQ = exmpp_iq:result(IQ_Rec, SubEl),
			    ejabberd_router:route(
			      exmpp_jid:make(User, 
                                     Server, 
                                     Resource),
			      exmpp_jid:make(User, 
                                     Server, 
                                     Resource),
			      exmpp_iq:iq_to_xmlel(ResIQ)),
			    ejabberd_auth:remove_user(User, Server),
			    ignore;
			_ ->
                            exmpp_iq:error(IQ_Rec, 'not-allowed')
		    end;
		(UTag /= undefined) and (PTag /= undefined) ->
		    User = exmpp_xml:get_cdata_as_list(UTag),
		    Password = exmpp_xml:get_cdata_as_list(PTag),
		    try_register_or_set_password(
		      User, Server, Password, From,
		      IQ_Rec, SubEl, Source, Lang, not IsCaptchaEnabled);
		IsCaptchaEnabled ->
		    case ejabberd_captcha:process_reply(SubEl) of
			ok ->
			    case process_xdata_submit(SubEl) of
				{ok, User, Password} ->
				    try_register_or_set_password(
				      User, Server, Password, From,
				      IQ_Rec, SubEl, Source, Lang, true);
				_ ->
				    exmpp_iq:error(IQ_Rec, 'bad-request')
			    end;
			{error, malformed} ->
			    exmpp_iq:error(IQ_Rec, 'bad-request');
			_ ->
			    ErrText = translate:translate(Lang, "Captcha test failed"),
			    exmpp_iq:error(IQ_Rec, 'not-allowed', ErrText)
			end;
		true ->
                    exmpp_iq:error(IQ_Rec, 'bad-request')
	    end;
	get ->
	    {IsRegistered, UsernameSubels, QuerySubels} =
		case {exmpp_jid:node_as_list(From), exmpp_jid:prep_domain_as_list(From)} of
		    {User, Server} when is_list(User) and is_list(Server) ->
			case ejabberd_auth:is_user_exists(User,Server) of
			    true ->
				{true, [#xmlcdata{cdata = list_to_binary(User)}],
				 [#xmlel{ns = ?NS_INBAND_REGISTER, name = 'registered'}]};
			    false ->
				{false, [#xmlcdata{cdata = list_to_binary(User)}], []}
			end;
		    _ ->
			{false, [], []}
		end,
	    if IsCaptchaEnabled and not IsRegistered ->
		    TopInstrEl =
			#xmlel{ns = ?NS_INBAND_REGISTER, name = 'instructions',
			       children =
				   [#xmlcdata{cdata =
						  list_to_binary(
						    translate:translate(Lang,
				      "You need an x:data capable client "
				      "with CAPTCHA support to register"))}]},
		    InstrEl =
			#xmlel{ns = ?NS_INBAND_REGISTER, name = 'instructions',
			       children =
				   [#xmlcdata{cdata =
						  list_to_binary(
						    translate:translate(Lang,
									"Choose a username and password "
									"to register with this server"))}]},
		    UField = #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
					[?XMLATTR(<<"var">>, <<"username">>),
					 ?XMLATTR(<<"type">>, <<"text-single">>),
					 ?XMLATTR(<<"label">>, translate:translate(Lang, "User"))],
				    children =
					[#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]},
		    PField =
			#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
				   [?XMLATTR(<<"var">>, <<"password">>),
				    ?XMLATTR(<<"type">>, <<"text-private">>),
				    ?XMLATTR(<<"label">>, translate:translate(Lang, "Password"))],
			       children = [
					   #xmlel{ns = ?NS_DATA_FORMS, name = 'required'}
					  ]
			      },
		    case ejabberd_captcha:create_captcha_x(
			   ID, To, Lang, [InstrEl, UField, PField]) of
			{ok, CaptchaEls} ->
				Result = #xmlel{ns = ?NS_INBAND_REGISTER, name = 'query', children = [TopInstrEl | CaptchaEls]},
				exmpp_iq:result(IQ_Rec, Result);
			error ->
			    ErrText = translate:translate(Lang, "Unable to generate a captcha"),
			    exmpp_iq:error(IQ_Rec, 'internal-server-error', ErrText)
		    end;
	       true ->
            Result = #xmlel{ns = ?NS_INBAND_REGISTER, name = 'query', children =
				[#xmlel{ns = ?NS_INBAND_REGISTER, name = 'instructions', children =
					    [#xmlcdata{cdata = list_to_binary(
								 translate:translate(Lang,
										     "Choose a username and password "
										     "to register with this server"))}]},
				 #xmlel{ns = ?NS_INBAND_REGISTER, name = 'username', children = UsernameSubels},
				 #xmlel{ns = ?NS_INBAND_REGISTER, name = 'password'}
				 | QuerySubels]},
            exmpp_iq:result(IQ_Rec, Result)
	    end
    end.

try_register_or_set_password(User, Server, Password, From, IQ_Rec,
			     SubEl, Source, Lang, CaptchaSucceed) ->
    case {exmpp_jid:node_as_list(From), exmpp_jid:prep_domain_as_list(From)} of
	{User, Server} ->
	    try_set_password(User, Server, Password, IQ_Rec, SubEl, Lang);
	_ when CaptchaSucceed ->
	    case check_from(From, Server) of
		allow ->
		    case try_register(User, Server, Password,
				      Source, Lang) of
			ok ->
			    exmpp_iq:result(IQ_Rec, SubEl);
			{error, Error} ->
			    exmpp_iq:error(IQ_Rec, Error)
		    end;
		deny ->
		    exmpp_iq:error(IQ_Rec, 'forbidden')
	    end;
	_ ->
	    exmpp_iq:error(IQ_Rec, 'not-allowed')
    end.

%% @doc Try to change password and return IQ response
try_set_password(User, Server, Password, IQ_Rec, SubEl, Lang) ->
    case is_strong_password(Server, Password) of
	true ->
		try_set_password_strong(User, Server, Password, IQ_Rec, SubEl, Lang);
	false ->
		ErrText = translate:translate(Lang, "The password is too weak"),
		exmpp_iq:error(IQ_Rec, 'not-acceptable', ErrText)
    end.

try_set_password_strong(User, Server, Password, IQ_Rec, SubEl, _Lang) ->
    case ejabberd_auth:set_password(User, Server, Password) of
	ok ->
            exmpp_iq:result(IQ_Rec, SubEl);
	{error, empty_password} ->
            exmpp_iq:error(IQ_Rec, 'bad-request');
	{error, not_allowed} ->
            exmpp_iq:error(IQ_Rec, 'not-allowed');
	{error, invalid_jid} ->
            exmpp_iq:error(IQ_Rec, 'item-not-found');
	_ ->
            exmpp_iq:error(IQ_Rec, 'internal-server-error')
    end.

try_register_strong(User, Server, Password, Source, _Lang, JID) ->
			    case ejabberd_auth:try_register(User, Server, Password) of
				{atomic, ok} ->
				    send_welcome_message(JID),
				    send_registration_notifications(JID, Source),
				    ok;
				Error ->
				    remove_timeout(Source),
				    case Error of
					{atomic, exists} ->
					    {error, 'conflict'};
					{error, invalid_jid} ->
					    {error, 'jid-malformed'};
					{error, not_allowed} ->
					    {error, 'not-allowed'};
					{error, _Reason} ->
					    {error, 'internal-server-error'}
				    end
				    end.

try_register(User, Server, Password, SourceRaw, Lang) ->
    case exmpp_stringprep:is_node(User) of
	false ->
	    {error, 'bad-request'};
	_ ->
	    JID = exmpp_jid:make(User, Server),
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	    IPAccess = get_ip_access(Server),
		case {acl:match_rule(Server, Access, JID), 
		  check_ip_access(SourceRaw, IPAccess)} of
		{deny, _} ->
		    {error, 'forbidden'};
		{_, deny} ->
		    {error, 'forbidden'};
		{allow, allow} ->
		    Source = may_remove_resource(SourceRaw),
		    case check_timeout(Source) of
			true ->
			    case is_strong_password(Server, Password) of
				true ->
					try_register_strong(User, Server, Password, Source, Lang, JID);
				false ->
				    ErrText = "The password is too weak",
				    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'not-acceptable', {Lang, ErrText})}
			    end;
			false ->
			    ErrText = "Users are not allowed to register "
				"accounts so quickly",
                            {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'resource-constraint', {Lang, ErrText})}
		    end
	    end
=======
    process_iq(From, To, IQ, jlib:jid_tolower(From)).

process_iq(From, To,
	   #iq{type = Type, lang = Lang, sub_el = SubEl, id = ID} =
	       IQ,
	   Source) ->
    IsCaptchaEnabled = case
			 gen_mod:get_module_opt(To#jid.lserver, ?MODULE,
						captcha_protected,
                                                fun(B) when is_boolean(B) -> B end,
                                                false)
			   of
			 true -> true;
			 _ -> false
		       end,
    case Type of
      set ->
	  UTag = xml:get_subtag(SubEl, <<"username">>),
	  PTag = xml:get_subtag(SubEl, <<"password">>),
	  RTag = xml:get_subtag(SubEl, <<"remove">>),
	  Server = To#jid.lserver,
	  Access = gen_mod:get_module_opt(Server, ?MODULE, access,
                                          fun(A) when is_atom(A) -> A end,
					  all),
	  AllowRemove = allow ==
			  acl:match_rule(Server, Access, From),
	  if (UTag /= false) and (RTag /= false) and
	       AllowRemove ->
		 User = xml:get_tag_cdata(UTag),
		 case From of
		   #jid{user = User, lserver = Server} ->
		       ejabberd_auth:remove_user(User, Server),
		       IQ#iq{type = result, sub_el = []};
		   _ ->
		       if PTag /= false ->
			      Password = xml:get_tag_cdata(PTag),
			      case ejabberd_auth:remove_user(User, Server,
							     Password)
				  of
				ok -> IQ#iq{type = result, sub_el = []};
				%% TODO FIXME: This piece of
				%% code does not work since
				%% the code have been changed
				%% to allow several auth
				%% modules.  lists:foreach can
				%% only return ok:
				not_allowed ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
				not_exists ->
				    IQ#iq{type = error,
					  sub_el =
					      [SubEl, ?ERR_ITEM_NOT_FOUND]};
				_ ->
				    IQ#iq{type = error,
					  sub_el =
					      [SubEl,
					       ?ERR_INTERNAL_SERVER_ERROR]}
			      end;
			  true ->
			      IQ#iq{type = error,
				    sub_el = [SubEl, ?ERR_BAD_REQUEST]}
		       end
		 end;
	     (UTag == false) and (RTag /= false) and AllowRemove ->
		 case From of
		   #jid{user = User, lserver = Server,
			resource = Resource} ->
		       ResIQ = #iq{type = result, xmlns = ?NS_REGISTER,
				   id = ID, sub_el = []},
		       ejabberd_router:route(jlib:make_jid(User, Server,
							   Resource),
					     jlib:make_jid(User, Server,
							   Resource),
					     jlib:iq_to_xml(ResIQ)),
		       ejabberd_auth:remove_user(User, Server),
		       ignore;
		   _ ->
		       IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		 end;
	     (UTag /= false) and (PTag /= false) ->
		 User = xml:get_tag_cdata(UTag),
		 Password = xml:get_tag_cdata(PTag),
		 try_register_or_set_password(User, Server, Password,
					      From, IQ, SubEl, Source, Lang,
					      not IsCaptchaEnabled);
	     IsCaptchaEnabled ->
		 case ejabberd_captcha:process_reply(SubEl) of
		   ok ->
		       case process_xdata_submit(SubEl) of
			 {ok, User, Password} ->
			     try_register_or_set_password(User, Server,
							  Password, From, IQ,
							  SubEl, Source, Lang,
							  true);
			 _ ->
			     IQ#iq{type = error,
				   sub_el = [SubEl, ?ERR_BAD_REQUEST]}
		       end;
		   {error, malformed} ->
		       IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]};
		   _ ->
		       ErrText = <<"The CAPTCHA verification has failed">>,
		       IQ#iq{type = error,
			     sub_el = [SubEl, ?ERRT_NOT_ALLOWED(Lang, ErrText)]}
		 end;
	     true ->
		 IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
	  end;
      get ->
	  {IsRegistered, UsernameSubels, QuerySubels} = case From
							    of
							  #jid{user = User,
							       lserver =
								   Server} ->
							      case
								ejabberd_auth:is_user_exists(User,
											     Server)
								  of
								true ->
								    {true,
								     [{xmlcdata,
								       User}],
								     [#xmlel{name
										 =
										 <<"registered">>,
									     attrs
										 =
										 [],
									     children
										 =
										 []}]};
								false ->
								    {false,
								     [{xmlcdata,
								       User}],
								     []}
							      end;
							  _ -> {false, [], []}
							end,
	  if IsCaptchaEnabled and not IsRegistered ->
		 TopInstrEl = #xmlel{name = <<"instructions">>,
				     attrs = [],
				     children =
					 [{xmlcdata,
					   translate:translate(Lang,
							       <<"You need a client that supports x:data "
								 "and CAPTCHA to register">>)}]},
		 InstrEl = #xmlel{name = <<"instructions">>, attrs = [],
				  children =
				      [{xmlcdata,
					translate:translate(Lang,
							    <<"Choose a username and password to register "
							      "with this server">>)}]},
		 UField = #xmlel{name = <<"field">>,
				 attrs =
				     [{<<"type">>, <<"text-single">>},
				      {<<"label">>,
				       translate:translate(Lang, <<"User">>)},
				      {<<"var">>, <<"username">>}],
				 children =
				     [#xmlel{name = <<"required">>, attrs = [],
					     children = []}]},
		 PField = #xmlel{name = <<"field">>,
				 attrs =
				     [{<<"type">>, <<"text-private">>},
				      {<<"label">>,
				       translate:translate(Lang,
							   <<"Password">>)},
				      {<<"var">>, <<"password">>}],
				 children =
				     [#xmlel{name = <<"required">>, attrs = [],
					     children = []}]},
		 case ejabberd_captcha:create_captcha_x(ID, To, Lang,
							Source,
							[InstrEl, UField,
							 PField])
		     of
		   {ok, CaptchaEls} ->
		       IQ#iq{type = result,
			     sub_el =
				 [#xmlel{name = <<"query">>,
					 attrs =
					     [{<<"xmlns">>,
					       <<"jabber:iq:register">>}],
					 children =
					     [TopInstrEl | CaptchaEls]}]};
		   {error, limit} ->
		       ErrText = <<"Too many CAPTCHA requests">>,
		       IQ#iq{type = error,
			     sub_el =
				 [SubEl,
				  ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)]};
		   _Err ->
		       ErrText = <<"Unable to generate a CAPTCHA">>,
		       IQ#iq{type = error,
			     sub_el =
				 [SubEl,
				  ?ERRT_INTERNAL_SERVER_ERROR(Lang, ErrText)]}
		 end;
	     true ->
		 IQ#iq{type = result,
		       sub_el =
			   [#xmlel{name = <<"query">>,
				   attrs =
				       [{<<"xmlns">>,
					 <<"jabber:iq:register">>}],
				   children =
				       [#xmlel{name = <<"instructions">>,
					       attrs = [],
					       children =
						   [{xmlcdata,
						     translate:translate(Lang,
									 <<"Choose a username and password to register "
									   "with this server">>)}]},
					#xmlel{name = <<"username">>,
					       attrs = [],
					       children = UsernameSubels},
					#xmlel{name = <<"password">>,
					       attrs = [], children = []}
					| QuerySubels]}]}
	  end
    end.

try_register_or_set_password(User, Server, Password,
			     From, IQ, SubEl, Source, Lang, CaptchaSucceed) ->
    case From of
      #jid{user = User, lserver = Server} ->
	  try_set_password(User, Server, Password, IQ, SubEl,
			   Lang);
      _ when CaptchaSucceed ->
	  case check_from(From, Server) of
	    allow ->
		case try_register(User, Server, Password, Source, Lang)
		    of
		  ok -> IQ#iq{type = result, sub_el = []};
		  {error, Error} ->
		      IQ#iq{type = error, sub_el = [SubEl, Error]}
		end;
	    deny ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
	  end;
      _ ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

%% @doc Try to change password and return IQ response
try_set_password(User, Server, Password, IQ, SubEl,
		 Lang) ->
    case is_strong_password(Server, Password) of
      true ->
	  case ejabberd_auth:set_password(User, Server, Password)
	      of
	    ok -> IQ#iq{type = result, sub_el = []};
	    {error, empty_password} ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]};
	    {error, not_allowed} ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	    {error, invalid_jid} ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
	    _ ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
	  end;
      false ->
	  ErrText = <<"The password is too weak">>,
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)]}
    end.

try_register(User, Server, Password, SourceRaw, Lang) ->
    case jlib:is_nodename(User) of
      false -> {error, ?ERR_BAD_REQUEST};
      _ ->
	  JID = jlib:make_jid(User, Server, <<"">>),
	  Access = gen_mod:get_module_opt(Server, ?MODULE, access,
                                          fun(A) when is_atom(A) -> A end,
					  all),
	  IPAccess = get_ip_access(Server),
	  case {acl:match_rule(Server, Access, JID),
		check_ip_access(SourceRaw, IPAccess)}
	      of
	    {deny, _} -> {error, ?ERR_FORBIDDEN};
	    {_, deny} -> {error, ?ERR_FORBIDDEN};
	    {allow, allow} ->
		Source = may_remove_resource(SourceRaw),
		case check_timeout(Source) of
		  true ->
		      case is_strong_password(Server, Password) of
			true ->
			    case ejabberd_auth:try_register(User, Server,
							    Password)
				of
			      {atomic, ok} ->
				  send_welcome_message(JID),
				  send_registration_notifications(
                                    ?MODULE, JID, Source),
				  ok;
			      Error ->
				  remove_timeout(Source),
				  case Error of
				    {atomic, exists} -> {error, ?ERR_CONFLICT};
				    {error, invalid_jid} ->
					{error, ?ERR_JID_MALFORMED};
				    {error, not_allowed} ->
					{error, ?ERR_NOT_ALLOWED};
				    {error, _Reason} ->
					{error, ?ERR_INTERNAL_SERVER_ERROR}
				  end
			    end;
			false ->
			    ErrText = <<"The password is too weak">>,
			    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
		      end;
		  false ->
		      ErrText =
			  <<"Users are not allowed to register accounts "
			    "so quickly">>,
		      {error, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)}
		end
	  end
>>>>>>> upstream/master
    end.

send_welcome_message(JID) ->
<<<<<<< HEAD
    Host = exmpp_jid:prep_domain_as_list(JID),
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message, {"", ""}) of
	{"", ""} ->
	    ok;
	{Subj, Body} ->
	    ejabberd_router:route(
	      exmpp_jid:make(Host),
	      JID,
              exmpp_message:normal(Subj, Body));
	_ ->
	    ok
    end.

send_registration_notifications(UJID, Source) ->
    Host = exmpp_jid:prep_domain_as_list(UJID),
    case gen_mod:get_module_opt(Host, ?MODULE, registration_watchers, []) of
	[] -> ok;
	JIDs when is_list(JIDs) ->
	    Body = lists:flatten(
		     io_lib:format(
		       "[~s] The account ~s was registered from IP address ~s "
		       "on node ~w using ~p.",
		       [get_time_string(), exmpp_jid:to_list(UJID),
			ip_to_string(Source), node(), ?MODULE])),
	    lists:foreach(
	      fun(S) ->
                      try
                          JID = exmpp_jid:parse(S),
                          ejabberd_router:route(
                            exmpp_jid:make(Host),
                            JID,
                            exmpp_message:chat(Body))
                      catch
                          _ ->
                              ok
		      end
	      end, JIDs);
	_ ->
	    ok
    end.

check_from(JID, Server) ->
    case {exmpp_jid:node(JID), exmpp_jid:prep_domain(JID)} of
	{undefined, undefined} ->
	    allow;
	_ ->
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access_from, none),
	    acl:match_rule(Server, Access, JID)
    end.
=======
    Host = JID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message,
                                fun(Opts) ->
                                        S = proplists:get_value(
                                              subject, Opts, <<>>),
                                        B = proplists:get_value(
                                              body, Opts, <<>>),
                                        {iolist_to_binary(S),
                                         iolist_to_binary(B)}
                                end, {<<"">>, <<"">>})
	of
      {<<"">>, <<"">>} -> ok;
      {Subj, Body} ->
	  ejabberd_router:route(jlib:make_jid(<<"">>, Host,
					      <<"">>),
				JID,
				#xmlel{name = <<"message">>,
				       attrs = [{<<"type">>, <<"normal">>}],
				       children =
					   [#xmlel{name = <<"subject">>,
						   attrs = [],
						   children =
						       [{xmlcdata, Subj}]},
					    #xmlel{name = <<"body">>,
						   attrs = [],
						   children =
						       [{xmlcdata, Body}]}]});
      _ -> ok
    end.

send_registration_notifications(Mod, UJID, Source) ->
    Host = UJID#jid.lserver,
    case gen_mod:get_module_opt(
           Host, Mod, registration_watchers,
           fun(Ss) ->
                   [#jid{} = jlib:string_to_jid(iolist_to_binary(S))
                    || S <- Ss]
           end, []) of
        [] -> ok;
        JIDs when is_list(JIDs) ->
            Body =
                iolist_to_binary(io_lib:format("[~s] The account ~s was registered from "
                                               "IP address ~s on node ~w using ~p.",
                                               [get_time_string(),
                                                jlib:jid_to_string(UJID),
                                                ip_to_string(Source), node(),
                                                Mod])),
            lists:foreach(
              fun(JID) ->
                      ejabberd_router:route(
                        jlib:make_jid(<<"">>, Host, <<"">>),
                        JID,
                        #xmlel{name = <<"message">>,
                               attrs = [{<<"type">>, <<"chat">>}],
                               children = [#xmlel{name = <<"body">>,
                                                  attrs = [],
                                                  children = [{xmlcdata,Body}]}]})
              end, JIDs)
    end.

check_from(#jid{user = <<"">>, server = <<"">>},
	   _Server) ->
    allow;
check_from(JID, Server) ->
    Access = gen_mod:get_module_opt(Server, ?MODULE, access_from,
                                    fun(A) when is_atom(A) -> A end,
                                    none),
    acl:match_rule(Server, Access, JID).
>>>>>>> upstream/master

check_timeout(undefined) -> true;
check_timeout(Source) ->
    Timeout = ejabberd_config:get_option(
                registration_timeout,
                fun(TO) when is_integer(TO), TO > 0 ->
                        TO;
                   (infinity) ->
                        infinity;
                   (unlimited) ->
                        infinity
                end, 600),
    if is_integer(Timeout) ->
	   {MSec, Sec, _USec} = now(),
	   Priority = -(MSec * 1000000 + Sec),
	   CleanPriority = Priority + Timeout,
	   F = fun () ->
		       Treap = case mnesia:read(mod_register_ip, treap, write)
				   of
				 [] -> treap:empty();
				 [{mod_register_ip, treap, T}] -> T
			       end,
		       Treap1 = clean_treap(Treap, CleanPriority),
		       case treap:lookup(Source, Treap1) of
			 error ->
			     Treap2 = treap:insert(Source, Priority, [],
						   Treap1),
			     mnesia:write({mod_register_ip, treap, Treap2}),
			     true;
			 {ok, _, _} ->
			     mnesia:write({mod_register_ip, treap, Treap1}),
			     false
		       end
	       end,
	   case mnesia:transaction(F) of
	     {atomic, Res} -> Res;
	     {aborted, Reason} ->
		 ?ERROR_MSG("mod_register: timeout check error: ~p~n",
			    [Reason]),
		 true
	   end;
       true -> true
    end.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
      true -> Treap;
      false ->
	  {_Key, Priority, _Value} = treap:get_root(Treap),
	  if Priority > CleanPriority ->
		 clean_treap(treap:delete_root(Treap), CleanPriority);
	     true -> Treap
	  end
    end.

remove_timeout(undefined) -> true;
remove_timeout(Source) ->
    Timeout = ejabberd_config:get_option(
                registration_timeout,
                fun(TO) when is_integer(TO), TO > 0 ->
                        TO;
                   (infinity) ->
                        infinity;
                   (unlimited) ->
                        infinity
                end, 600),
    if is_integer(Timeout) ->
	   F = fun () ->
		       Treap = case mnesia:read(mod_register_ip, treap, write)
				   of
				 [] -> treap:empty();
				 [{mod_register_ip, treap, T}] -> T
			       end,
		       Treap1 = treap:delete(Source, Treap),
		       mnesia:write({mod_register_ip, treap, Treap1}),
		       ok
	       end,
	   case mnesia:transaction(F) of
	     {atomic, ok} -> ok;
	     {aborted, Reason} ->
		 ?ERROR_MSG("mod_register: timeout remove error: "
			    "~p~n",
			    [Reason]),
		 ok
	   end;
       true -> ok
    end.

ip_to_string(Source) when is_tuple(Source) ->
    jlib:ip_to_list(Source);
ip_to_string(undefined) -> <<"undefined">>;
ip_to_string(_) -> <<"unknown">>.

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized

write_time({{Y, Mo, D}, {H, Mi, S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
		  [Y, Mo, D, H, Mi, S]).

process_xdata_submit(El) ->
    case xml:get_subtag(El, <<"x">>) of
      false -> error;
      Xdata ->
	  Fields = jlib:parse_xdata_submit(Xdata),
	  case catch {proplists:get_value(<<"username">>, Fields),
		      proplists:get_value(<<"password">>, Fields)}
	      of
	    {[User | _], [Pass | _]} -> {ok, User, Pass};
	    _ -> error
	  end
    end.

is_strong_password(Server, Password) ->
    LServer = jlib:nameprep(Server),
    case gen_mod:get_module_opt(LServer, ?MODULE, password_strength,
                                fun(N) when is_number(N), N>=0 -> N end,
                                0) of
        0 ->
            true;
        Entropy ->
            ejabberd_auth:entropy(Password) >= Entropy
    end.

transform_options(Opts) ->
    Opts1 = transform_ip_access(Opts),
    transform_module_options(Opts1).

transform_ip_access(Opts) ->
    try
        {value, {modules, ModOpts}, Opts1} = lists:keytake(modules, 1, Opts),
        {value, {?MODULE, RegOpts}, ModOpts1} = lists:keytake(?MODULE, 1, ModOpts),
        {value, {ip_access, L}, RegOpts1} = lists:keytake(ip_access, 1, RegOpts),
        true = is_list(L),
        ?WARNING_MSG("Old 'ip_access' format detected. "
                     "The old format is still supported "
                     "but it is better to fix your config: "
                     "use access rules instead.", []),
        ACLs = lists:flatmap(
                 fun({Action, S}) ->
                         ACLName = jlib:binary_to_atom(
                                     iolist_to_binary(
                                       ["ip_", S])),
                         [{Action, ACLName},
                          {acl, ACLName, {ip, S}}]
                 end, L),
        Access = {access, mod_register_networks,
                  [{Action, ACLName} || {Action, ACLName} <- ACLs]},
        [ACL || {acl, _, _} = ACL <- ACLs] ++
            [Access,
             {modules,
              [{mod_register,
                [{ip_access, mod_register_networks}|RegOpts1]}
               | ModOpts1]}|Opts1]
    catch error:{badmatch, false} ->
            Opts
    end.

<<<<<<< HEAD
ip_to_string(Source) when is_tuple(Source) -> inet_parse:ntoa(Source);
ip_to_string(undefined) -> "undefined";
ip_to_string(_) -> "unknown".

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized
write_time({{Y,Mo,D},{H,Mi,S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
		  [Y, Mo, D, H, Mi, S]).

process_xdata_submit(El) ->
    case exmpp_xml:get_element(El, x) of
        #xmlel{} = Xdata ->
            Fields = jlib:parse_xdata_submit(Xdata),
            case catch {proplists:get_value("username", Fields),
			proplists:get_value("password", Fields)} of
                {[User|_], [Pass|_]} ->
		    {ok, User, Pass};
		_ ->
		    error
	    end;
        _ ->
	    error
    end.

is_strong_password(Server, Password) ->
    LServer = exmpp_stringprep:nameprep(Server),
    case gen_mod:get_module_opt(LServer, ?MODULE, password_strength, 0) of
	Entropy when is_number(Entropy), Entropy >= 0 ->
	    if Entropy == 0 ->
		    true;
	       true ->
		    ejabberd_auth:entropy(Password) >= Entropy
	    end;
	Wrong ->
	    ?WARNING_MSG("Wrong value for password_strength option: ~p",
			 [Wrong]),
	    true
    end.
=======
transform_module_options(Opts) ->
    lists:flatmap(
      fun({welcome_message, {Subj, Body}}) ->
              ?WARNING_MSG("Old 'welcome_message' format detected. "
                           "The old format is still supported "
                           "but it is better to fix your config: "
                           "change it to {welcome_message, "
                           "[{subject, Subject}, {body, Body}]}",
                           []),
              [{welcome_message, [{subject, Subj}, {body, Body}]}];
         (Opt) ->
              [Opt]
      end, Opts).
>>>>>>> upstream/master

%%%
%%% ip_access management
%%%

<<<<<<< HEAD
may_remove_resource({U, S, _}) ->
	{U, S, ""};
may_remove_resource(From) ->
    From.

get_ip_access(Host) ->
    IPAccess = gen_mod:get_module_opt(Host, ?MODULE, ip_access, []),
    lists:flatmap(
      fun({Access, S}) ->
	      case parse_ip_netmask(S) of
		  {ok, IP, Mask} ->
		      [{Access, IP, Mask}];
		  error ->
		      ?ERROR_MSG("mod_register: invalid "
				 "network specification: ~p",
				 [S]),
		      []
	      end
      end, IPAccess).

parse_ip_netmask(S) ->
    case string:tokens(S, "/") of
	[IPStr] ->
	    case inet_parse:address(IPStr) of
		{ok, {_, _, _, _} = IP} ->
		    {ok, IP, 32};
		{ok, {_, _, _, _, _, _, _, _} = IP} ->
		    {ok, IP, 128};
		_ ->
		    error
	    end;
	[IPStr, MaskStr] ->
	    case catch list_to_integer(MaskStr) of
		Mask when is_integer(Mask),
			  Mask >= 0 ->
		    case inet_parse:address(IPStr) of
			{ok, {_, _, _, _} = IP} when Mask =< 32 ->
			    {ok, IP, Mask};
			{ok, {_, _, _, _, _, _, _, _} = IP} when Mask =< 128 ->
			    {ok, IP, Mask};
			_ ->
			    error
		    end;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

check_ip_access(_Source, []) ->
    allow;
check_ip_access({User, Server, Resource}, IPAccess) ->
    JID = exmpp_jid:make(User, Server, Resource),
    case ejabberd_sm:get_user_ip(JID) of
	{IPAddress, _PortNumber} -> check_ip_access(IPAddress, IPAccess);
	_ -> true
    end;
check_ip_access({_, _, _, _} = IP,
		[{Access, {_, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (32 - Mask)) - 1),
    if
	IPInt band M =:= NetInt band M ->
	    Access;
	true ->
	    check_ip_access(IP, IPAccess)
    end;
check_ip_access({_, _, _, _, _, _, _, _} = IP,
		[{Access, {_, _, _, _, _, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (128 - Mask)) - 1),
    if
	IPInt band M =:= NetInt band M ->
	    Access;
	true ->
	    check_ip_access(IP, IPAccess)
    end;
check_ip_access(IP, [_ | IPAccess]) ->
    check_ip_access(IP, IPAccess).

ip_to_integer({IP1, IP2, IP3, IP4}) ->
    (((((IP1 bsl 8) bor IP2) bsl 8) bor IP3) bsl 8) bor IP4;
ip_to_integer({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}) ->
    (((((((((((((IP1 bsl 16) bor IP2) bsl 16) bor IP3) bsl 16) bor IP4)
	   bsl 16) bor IP5) bsl 16) bor IP6) bsl 16) bor IP7) bsl 16) bor IP8.
=======
may_remove_resource({_, _, _} = From) ->
    jlib:jid_remove_resource(From);
may_remove_resource(From) -> From.

get_ip_access(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, ip_access,
                           fun(A) when is_atom(A) -> A end,
                           all).

check_ip_access({User, Server, Resource}, IPAccess) ->
    case ejabberd_sm:get_user_ip(User, Server, Resource) of
        {IPAddress, _PortNumber} ->
            check_ip_access(IPAddress, IPAccess);
        _ ->
            deny
    end;
check_ip_access(undefined, _IPAccess) ->
    deny;
check_ip_access(IPAddress, IPAccess) ->
    acl:match_rule(global, IPAccess, IPAddress).
>>>>>>> upstream/master
