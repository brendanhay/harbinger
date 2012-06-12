%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_topic).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("riak_zab_vnode.hrl").

%% API
-export([publish/2,
         bind/2,
         unbind/2]).

%%
%% API
%%

-spec publish(binary(), binary()) -> ok.
%% @doc
publish(Topic, Payload) ->
    %% Write the payload to the topic and then the vnodes call
    %% queue:handle_publish offset reply
    ok.

-spec bind(binary(), binary()) -> {ok, NewestOffset, OldestOffset}.
%% @doc
bind(Topic, Queue) ->
    {ok, NewestOffset, OldestOffset}.

-spec unbind(binary(), binary()) -> ok.
%% @doc
unbind(Topic, Queue) ->
    ok.

%%
%% Private
%%
