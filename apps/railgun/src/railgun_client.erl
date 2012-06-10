%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_client).

-include("harbinger.hrl").

%% API
-export([publish/3]).

%%
%% API
%%

-spec publish(binary(), binary(), binary()) -> ok.
%% @doc
publish(Topic, Queue, Msg) ->
    {ok, ReqId, PrefList} =
        harbinger_coordinator:prepare({Topic, Queue}, harbinger_queue),
    ok = harbinger_queue_vnode:enqueue(PrefList, ReqId, Topic, Queue, Msg),
    harbinger_coordinator:wait(ReqId, self()).

%%
%% Private
%%
