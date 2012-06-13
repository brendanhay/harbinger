%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_queue_vnode).

-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("riak_zab_vnode.hrl").
-include("harbinger.hrl").

%% API
-export([hash_key/1,
         subscribe/2,
         unsubscribe/1,
         notify/3]).

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

-record(state, {logs :: dict()}).

%%
%% API
%%

%%
%% API
%%

-spec subscribe(binary(), binary()) -> ok.
%% @doc
subscribe(_Topic, _Queue) ->
    %% {ok, NewestOffset, OldestOffset} = harbinger_topic:bind(Topic, Queue),
    %% Notify ensemble of new connection/subscription
    ok.

-spec unsubscribe(binary()) -> ok.
%% @doc
unsubscribe(_Queue) ->
    %% ok = harbinger_topic:unbind(Topic, Queue),
    %% Now what, crash all the connected connections?
    ok.

%% -spec handle_publish(binary(), pos_integer()) -> ok.
%% @doc
notify(Topic, Queue, {Head, Tail}) ->
    riak_zab_util:command(Queue, {bound, Topic, Head, Tail}, ?QUEUE_MASTER),
    %% Notify the ensemble of the updated offset
    ok.

hash_key(Key) -> chash:key_of(Key).

%%
%% Callbacks
%%

start_vnode(Index) -> riak_core_vnode_master:get_vnode_pid(Index, ?MODULE).

init([_Partition]) -> {ok, #state{logs=dict:new()}}.

handle_command(?ZAB_SYNC{peer=Peer, idxs=Idxs}, _Sender, State) ->
    io:format("V: Synchronizing ~p with ~p :: ~p~n", [self(), Peer, Idxs]),
    riak_zab_vnode:standard_sync(?MODULE, State, Peer, Idxs),
    {reply, ok, State};

handle_command(?ZAB_SYNC_DATA{data={K,V}}, _Sender, State=#state{logs=Logs}) ->
    io:format("Received sync message~n", []),
    io:format("Log(~p) = ~p~n", [K, V]),
    Logs2 = dict:store(K, V, Logs),
    {reply, ok, State#state{logs=Logs2}};

handle_command(?ZAB_REQ{req=Req, zxid=Zxid, sender=Sender, leading=Leading},
               _Master, State) ->
    handle_zab_command(Req, Zxid, Leading, Sender, State);

handle_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State=#state{logs=Logs}) ->
    Reply = dict:fold(Fun, Acc0, Logs),
    {reply, Reply, State};

handle_command(_Cmd, _Sender, State) ->
    {noreply, State}.

handle_zab_command(Msg = {bound, _Topic, _Head, _Tail}, Zxid, Leading, _Sender, State) ->
    io:format("Received store~n"
              "     Zxid: ~p~n"
              "  Leading: ~p~n"
              "      Msg: ~p~n",  [Zxid, Leading, Msg]),
    {reply, ok, State};
handle_zab_command({store, Key, Msg}, Zxid, Leading, _Sender,
                   State=#state{logs=Logs}) ->
    Logs2 = dict:append(Key, Msg, Logs),
    io:format("Received store~n"
              "     Zxid: ~p~n"
              "  Leading: ~p~n"
              "      Key: ~p~n"
              "      Msg: ~p~n",  [Zxid, Leading, Key, Msg]),
    io:format("Logs(~p) = ~p~n", [Key, dict:fetch(Key, Logs2)]),
    {reply, ok, State#state{logs=Logs2}};
handle_zab_command({get, Key}, Zxid, Leading, Sender,
                   State=#state{logs=Logs}) ->
    Res = dict:find(Key, Logs),
    io:format("Received get~n"
              "     Zxid: ~p~n"
              "  Leading: ~p~n"
              "      Key: ~p~n", [Zxid, Leading, Key]),
    io:format("Logs(~p) = ~p~n", [Key, Res]),
    maybe_reply(Leading, Sender, Res),
    {reply, ok, State};
handle_zab_command(_Req, _Zxid, _Leading, _Sender, State) ->
    {noreply, State}.

handle_handoff_command(Req=?FOLD_REQ{}, Sender, State) ->
    handle_command(Req, Sender, State);
handle_handoff_command(_Cmd, _Sender, State) ->
    {ok, State}.

handoff_starting(_Node, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(BinObj, State=#state{logs=Logs}) ->
    {K, V} = binary_to_term(BinObj),
    Logs2 = dict:store(K, V, Logs),
    {reply, ok, State#state{logs=Logs2}}.

encode_handoff_item(K, V) ->
    term_to_binary({K,V}).

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

is_empty(State=#state{logs=Logs}) ->
    Empty = (dict:size(Logs) == 0),
    {Empty, State}.

delete(State) ->
    {ok, State#state{logs=dict:new()}}.

terminate(_Reason, _State) ->
    ok.

%%
%% Private
%%

maybe_reply(true, Sender, Res) ->
    io:format("Sending reply to ~p :: ~p~n", [Sender, Res]),
    riak_zab_vnode:reply(Sender, Res),
    ok;
maybe_reply(false, _Sender, _Res) ->
    ok.
