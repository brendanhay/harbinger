%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(railgun_publisher).

-behaviour(gen_fsm).

-include("railgun.hrl").

%% API
-export([start_link/5,
         publish/3]).

%% Callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% States
-export([prepare/2,
         execute/2,
         waiting/2]).

%%
%% Types
%%

-record(s, {req_id            :: pos_integer(),
            from              :: pid(),
            topic             :: binary(),
            queue             :: binary(),
            msg = undefined   :: binary(),
            preflist          :: riak_core_apl:preflist2(),
            write_replies = 0 :: non_neg_integer()}).

%%
%% API
%%

start_link(ReqId, From, Topic, Queue, Msg) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Topic, Queue, Msg], []).

publish(Topic, Queue, Msg) ->
    ReqId = generate_request_id(),
    railgun_publisher_sup:start_publisher([ReqId, self(), Topic, Queue, Msg]),
    {ok, ReqId}.

%%
%% Callbacks
%%

%% @doc Initialize the state data.
init([ReqId, From, Topic, Queue, Msg]) ->
    State = #s{req_id = ReqId,
               from   = From,
               topic  = Topic,
               queue  = Queue,
               msg    = Msg},
    {ok, prepare, State, 0}.

handle_info(_Info, _StateName, State) -> {stop, badmsg, State}.

handle_event(_Event, _StateName, State) -> {stop, badmsg, State}.

handle_sync_event(_Event, _From, _StateName, State) -> {stop, badmsg, State}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _StateName, _State) -> ok.

%%
%% States
%%

%% @doc Prepare the publish by calculating the _preference list_.
prepare(timeout, State = #s{topic = Topic, queue = Queue}) ->
    DocIdx = riak_core_util:chash_key({Topic, Queue}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, railgun_queue_vnode),
    {next_state, execute, State#s{preflist = PrefList}, 0}.

%% @doc Execute the enqueue request and then go into waiting state to
%% verify it has meets consistency requirements.
execute(timeout, State = #s{req_id   = ReqId,
                            topic    = Topic,
                            queue    = Queue,
                            msg      = Msg,
                            preflist = Preflist}) ->
    railgun_queue_vnode:enqueue(Preflist, ReqId, Topic, Queue, Msg),
    {next_state, waiting, State}.

%% @doc Wait for W enqueue reqs to respond.
waiting({ok, ReqId}, State = #s{from = From, write_replies = W}) ->
    IncW = W + 1,
    NewState = State#s{ write_replies = IncW },
    if
        IncW =:= ?W ->
            From ! {ReqId, ok},
            {stop, normal, NewState};
        true ->
            {next_state, waiting, NewState}
    end.

%%
%% Private
%%

generate_request_id() -> erlang:phash2(erlang:now()).
