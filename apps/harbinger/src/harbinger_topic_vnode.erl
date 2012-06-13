%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_topic_vnode).

-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("harbinger.hrl").

%% API
-export([publish/2,
         bindings/1,
         bind/2,
         unbind/2]).

%% Callbacks
-export([start_vnode/1,
         init/1,
         handle_command/3,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3,
         is_empty/1,
         delete/1,
         terminate/2]).

%%
%% Types
%%

-record(s, {partition = 0               :: non_neg_integer(),
            head      = 0               :: non_neg_integer(),
            tail      = 0               :: non_neg_integer(),
            bindings  = gb_sets:empty() :: gb_set(),
            log       = []              :: [binary()]}).

-define(TIMEOUT, 5000).

%%
%% API
%%

-spec publish(binary(), binary()) -> {ok, {non_neg_integer(), non_neg_integer()}}.
%% @doc
publish(Topic, Payload) ->
    wait(Topic, fun(R) -> {publish, R, Topic, Payload} end, reply).

-spec bind(binary(), binary()) -> {ok, {non_neg_integer(), non_neg_integer()}}.
%% @doc
bind(Topic, Queue) ->
    wait(Topic, fun(R) -> {bind, R, Topic, Queue} end, reply).

-spec unbind(binary(), binary()) -> ok.
%% @doc
unbind(Topic, Queue) ->
    wait(Topic, fun(R) -> {unbind, R, Topic, Queue} end, noreply).

-spec bindings(binary()) -> {ok, [{queue, binary()}]}.
%% @doc
bindings(Topic) ->
    wait(Topic, fun(R) -> {bindings, R, Topic} end, reply).

%%
%% Callbacks
%%

start_vnode(Index) -> riak_core_vnode_master:get_vnode_pid(Index, ?MODULE).

init([Partition]) -> {ok, #s{partition = Partition}}.

%% Publish
handle_command({publish, ReqId, _Topic, Payload}, _Sender, State = #s{tail = Tail, log = Log}) ->
    NewState = State#s{log = [Payload|Log], tail = Tail + 1},
    {reply, {ok, ReqId, {State#s.head, State#s.tail}}, NewState};

%% Bind
handle_command({bind, ReqId, Topic, Queue}, _Sender, State = #s{bindings = Bindings}) ->
    NewState = State#s{bindings = gb_sets:add(Queue, Bindings)},
    {reply, {ok, ReqId, {State#s.head, State#s.tail}}, NewState};

%% Unbind
handle_command({unbind, ReqId, _Topic, Queue}, _Sender, State = #s{bindings = Bindings}) ->
    NewState = State#s{bindings = gb_sets:delete_any(Queue, Bindings)},
    {reply, {ok, ReqId}, NewState};

%% Bindings
handle_command({bindings, ReqId, _Topic}, _Sender, State) ->
    {reply, {ok, ReqId, gb_sets:to_list(State#s.bindings)}, State};

%% Ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#s.partition}, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Private
%%

%% @private
wait(Topic, CmdFun, Mode) ->
    harbinger_coordinator:wait({Topic, Topic},
                               harbinger_topic,
                               CmdFun,
                               ?TOPIC_MASTER,
                               Mode).
