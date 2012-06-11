%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
%% @hidden
init(_Args) ->
    Port = case application:get_env(harbinger, stomp_port) of
               {ok, P} -> P;
               _Other  -> 5600
           end,
    Specs = [{listener,
              {harbinger_listener, start_link, ["0.0.0.0", Port]},
              permanent, 5000, worker, [harbinger_listener]},
             {connection_master_sup,
              {harbinger_connection_sup, start_link, []},
              permanent, 5000, worker, [harbinger_connection_sup]},
             {topic_vnode_master,
              {riak_core_vnode_master, start_link, [harbinger_topic_vnode]},
              permanent, 5000, worker, [riak_core_vnode_master]},
             {queue_vnode_master,
              {riak_core_vnode_master, start_link, [harbinger_queue_vnode]},
              permanent, 5000, worker, [riak_core_vnode_master]}],
    {ok, {{one_for_one, 5, 10}, Specs}}.
