%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_coordinator).

-include("harbinger.hrl").

%% API
-export([prepare/2,
         wait/2,
         wait/3]).

%%
%% Types
%%

-type request_id() :: pos_integer().

-define(TIMEOUT, 5000).

%%
%% API
%%

-spec prepare(riak_object:bkey(), atom())
             -> {ok, request_id(), riak_core_apl:preflist()}.
%% @doc Prepare the required components for a vnode service request
prepare(Key, Service) -> {ok, mk_request_id(), get_preflist(Key, Service)}.

-spec wait(request_id(), pid()) -> ok.
%% @private Wait for the default number of requests to reply
wait(ReqId, From) -> wait(ReqId, From, ?W).

-spec wait(request_id(), pid(), non_neg_integer()) -> ok.
%% @private Wait for consistency W requests to reply
wait(ReqId, From, 0) ->
    From ! {ReqId, ok},
    ok;
wait(ReqId, From, W) ->
    receive
        {undefined, {ok, ReqId}} -> wait(ReqId, From, W - 1)
    after
        ?TIMEOUT                 -> error({coordinator_wait, timeout_elapsed})
    end.

%%
%% Private
%%

-spec mk_request_id() -> request_id().
%% @private Generate a sudo random request id
mk_request_id() -> erlang:phash2(erlang:now()).

-spec get_preflist(riak_object:bkey(), atom()) -> riak_core_apl:preflist().
%% @private Calculate the preference list for the vnode service
get_preflist(Key, Service) ->
    DocIdx = riak_core_util:chash_key(Key),
    riak_core_apl:get_apl(DocIdx, ?N, Service).
