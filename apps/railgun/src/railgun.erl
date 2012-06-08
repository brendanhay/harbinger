%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(railgun).

-behaviour(application).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("railgun.hrl").

%% API
-export([start/0,
         stop/0,
         topic/0,
         queue/0,
         queue/2]).

%% Callbacks
-export([start/2,
         stop/1]).

%%
%% API
%%

-spec start() -> ok.
%% @doc
start() -> start(?MODULE).

-spec stop() -> ok.
%% @doc
stop() ->
    ok = application:stop(?MODULE),
    init:stop().

%% @doc Ping a random topic vnode
topic() ->
    ping({<<"ping">>, term_to_binary(now())}, railgun_topic_vnode_master).

%% @doc Ping a random queue vnode
queue() ->
    ping({<<"ping">>, term_to_binary(now())}, railgun_queue_vnode_master).

%% @doc Ping a specific queue vnode
queue(Topic, Queue) ->
    ping({list_to_binary(Topic), list_to_binary(Queue)},
         railgun_queue_vnode_master).

%%
%% Callbacks
%%

-spec start(normal, _) -> {ok, pid()} | {error, _}.
%% @hidden
start(_StartType, _StartArgs) ->
    case railgun_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, railgun_topic_vnode}]),
            ok = riak_core:register([{vnode_module, railgun_queue_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(railgun_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(railgun_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(railgun, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(_) -> ok.
%% @hidden
stop(_Args) -> ok.

%%
%% Private
%%

ping(Key, VNode) ->
    DocIdx = riak_core_util:chash_key(Key),
    [{IndexNode, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, railgun),
    lager:info("IndexNode: ~p", [IndexNode]),
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, VNode).

-spec start(atom()) -> ok.
%% @doc
start(App) -> ensure_started(App, application:start(App, permanent)).

-spec ensure_started(atom(), ok | {error, {already_started, atom()} |
                                   {not_started, atom()}}) -> ok.
%% @private
ensure_started(_App, ok) ->
    ok;
ensure_started(_App, {error, {already_started, _App}}) ->
    ok;
ensure_started(App, {error, {not_started, Dep}}) ->
    start(Dep),
    start(App);
ensure_started(App, {error, Reason}) ->
    lager:error("Application '~s' start failed with ~p", [App, Reason]),
    error({app_start_failed, App, Reason}).
