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
-export([reply/4,
         noreply/4]).

%%
%% Types
%%

-type request_id() :: pos_integer().

-define(TIMEOUT, 5000).

%%
%% API
%%

reply(Key, Service, CommandFun, VNodeMaster) ->
    command(Key, Service, CommandFun, VNodeMaster, fun wait_reply/2).

noreply(Key, Service, CommandFun, VNodeMaster) ->
    command(Key, Service, CommandFun, VNodeMaster, fun wait_noreply/2).

%%
%% Private
%%

command(Key, Service, CommandFun, VNodeMaster, ReplyFun) ->
    ReqId = mk_request_id(),
    riak_core_vnode_master:command(get_preflist(Key, Service),
                                   CommandFun(ReqId),
                                   ?REPLY_SELF,
                                   VNodeMaster),
    ReplyFun(ReqId, self()).

-spec mk_request_id() -> request_id().
%% @private Generate a sudo random request id
mk_request_id() -> erlang:phash2(erlang:now()).

-spec get_preflist(riak_object:bkey(), atom()) -> riak_core_apl:preflist().
%% @private Calculate the preference list for the vnode service
get_preflist(Key, Service) ->
    DocIdx = riak_core_util:chash_key(Key),
    riak_core_apl:get_apl(DocIdx, ?N, Service).

-spec wait_noreply(request_id(), pid()) -> ok.
%% @private Wait_Noreply for the default number of requests to reply
wait_noreply(ReqId, From) -> wait_noreply(ReqId, From, ?W).

-spec wait_noreply(request_id(), pid(), non_neg_integer()) -> ok.
%% @private Wait_Noreply for consistency W requests to reply
wait_noreply(ReqId, From, 0) ->
    From ! {ReqId, ok},
    ok;
wait_noreply(ReqId, From, W) ->
    receive
        {undefined, {ok, ReqId}} -> wait_noreply(ReqId, From, W - 1)
    after
        ?TIMEOUT                 -> error({coordinator_wait_noreply, timeout_elapsed})
    end.

wait_reply(ReqId, From) -> wait_reply(ReqId, From, ?W, []).

wait_reply(ReqId, From, 0, Acc) ->
    From ! {ReqId, ok},
    {ok, Acc};
wait_reply(ReqId, From, W, Acc) ->
    receive
        {undefined, {ok, ReqId, Wait_Reply}} ->
            wait_reply(ReqId, From, W - 1, [Wait_Reply|Acc])
    after
        ?TIMEOUT ->
            error({coordinator_wait_reply, timeout_elapsed})
    end.
