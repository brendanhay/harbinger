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
-export([wait/5]).

%%
%% Types
%%

-type request_id() :: pos_integer().

-define(TIMEOUT, 5000).

%%
%% API
%%

wait(Key, Service, CmdFun, VMaster, reply) ->
    command(Key, Service, CmdFun, VMaster, fun reply/2);
wait(Key, Service, CmdFun, VMaster, noreply) ->
    command(Key, Service, CmdFun, VMaster, fun noreply/2).

%%
%% Private
%%

command(Key, Service, CmdFun, VMaster, ReplyFun) ->
    ReqId = mk_request_id(),
    riak_core_vnode_master:command(get_preflist(Key, Service),
                                   CmdFun(ReqId),
                                   ?REPLY_SELF,
                                   VMaster),
    ReplyFun(ReqId, self()).

-spec mk_request_id() -> request_id().
%% @private Generate a sudo random request id
mk_request_id() -> erlang:phash2(erlang:now()).

-spec get_preflist(riak_object:bkey(), atom()) -> riak_core_apl:preflist().
%% @private Calculate the preference list for the vnode service
get_preflist(Key, Service) ->
    DocIdx = riak_core_util:chash_key(Key),
    riak_core_apl:get_apl(DocIdx, ?N, Service).

-spec noreply(request_id(), pid()) -> ok.
%% @private Noreply for the default number of requests to reply
noreply(ReqId, From) -> noreply(ReqId, From, ?W).

-spec noreply(request_id(), pid(), non_neg_integer()) -> ok.
%% @private Noreply for consistency W requests to reply
noreply(ReqId, From, 0) ->
    From ! {ReqId, ok},
    ok;
noreply(ReqId, From, W) ->
    receive
        {undefined, {ok, ReqId}} ->
            noreply(ReqId, From, W - 1)
    after
        ?TIMEOUT ->
            error({coordinator_noreply, timeout_elapsed})
    end.

reply(ReqId, From) -> reply(ReqId, From, ?W, []).

reply(ReqId, From, 0, Acc) ->
    From ! {ReqId, ok},
    {ok, Acc};
reply(ReqId, From, W, Acc) ->
    receive
        {undefined, {ok, ReqId, Reply}} ->
            reply(ReqId, From, W - 1, [Reply|Acc])
    after
        ?TIMEOUT ->
            error({coordinator_reply, timeout_elapsed})
    end.
