%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_queue).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("riak_zab_vnode.hrl").

%% API
-export([subscribe/2,
         unsubscribe/1,
         handle_publish/2]).

%%
%% API
%%

-spec subscribe(binary(), binary()) -> ok.
%% @doc
subscribe(Topic, Queue) ->
    {ok, NewestOffset, OldestOffset} = harbinger_topic:bind(Topic, Queue),
    %% Notify ensemble of new connection/subscription
    ok.

-spec unsubscribe(binary()) -> ok.
%% @doc
unsubscribe(Queue) ->
    ok = harbinger_topic:unbind(Topic, Queue),
    %% Now what, crash all the connected connections?
    ok.

-spec handle_publish(binary(), pos_integer()) -> ok.
%% @doc
handle_publish(Queue, NewestOffset) ->
    %% Notify the ensemble of the updated offset
    ok.

%%
%% Private
%%
