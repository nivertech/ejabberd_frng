%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
<<<<<<< HEAD:src/mod_pubsub/pubsub_index.erl
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%%
%%% @copyright 2006-2012 ProcessOne
=======
%%% Portions created by ProcessOne are Copyright 2006-2014, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2014, ProcessOne.
%%%
%%%
%%% @copyright 2006-2014 ProcessOne
>>>>>>> upstream/master:src/pubsub_index.erl
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%% important note:
%% new/1 and free/2 MUST be called inside a transaction bloc

-module(pubsub_index).

-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-export([
	 init/3,
	 new/1,
	 free/2
	]).


-spec(init/3 ::
      (
	     Host       :: string(),
	     ServerHost :: string(),
	     Opts       :: [{Key::atom(), Value::term()}])
      -> 'ok'
	    ).

init(_Host, _ServerHost, _Opts) ->
    mnesia:create_table(pubsub_index,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, pubsub_index)}]).


-spec(new/1 ::
      (
	    Index::atom())
      -> Idx::integer()
	    ).

new(Index) ->
    case mnesia:read({pubsub_index, Index}) of
<<<<<<< HEAD:src/mod_pubsub/pubsub_index.erl
	[#pubsub_index{free = [], last = Last} = PubsubIndex] ->
	    Idx = Last + 1,
	    mnesia:write(PubsubIndex#pubsub_index{last = Idx}),
	    Idx;
	[#pubsub_index{free = [Idx|Free]} = PubsubIndex] ->
	    mnesia:write(PubsubIndex#pubsub_index{free = Free}),
	    Idx;
	_ ->
	    mnesia:write(#pubsub_index{index = Index, last = 1, free = []}),
	    1
=======
      [I] ->
	  case I#pubsub_index.free of
	    [] ->
		Id = I#pubsub_index.last + 1,
		mnesia:write(I#pubsub_index{last = Id}),
		Id;
	    [Id | Free] ->
		mnesia:write(I#pubsub_index{free = Free}), Id
	  end;
      _ ->
	  mnesia:write(#pubsub_index{index = Index, last = 1,
				     free = []}),
	  1
>>>>>>> upstream/master:src/pubsub_index.erl
    end.


-spec(free/2 ::
      (
	     Index :: atom(),
	     Idx   :: integer())
      -> 'ok'
	    ).

free(Index, Idx) ->
    case mnesia:read({pubsub_index, Index}) of
<<<<<<< HEAD:src/mod_pubsub/pubsub_index.erl
	[#pubsub_index{free = Free} = PubsubIndex] ->
	    mnesia:write(PubsubIndex#pubsub_index{free = [Idx|Free]});
	_ -> ok
=======
      [I] ->
	  Free = I#pubsub_index.free,
	  mnesia:write(I#pubsub_index{free = [Id | Free]});
      _ -> ok
>>>>>>> upstream/master:src/pubsub_index.erl
    end.
