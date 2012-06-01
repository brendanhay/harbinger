%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(cap_connection).

-behaviour(plain_fsm).
-behaviour(cowboy_protocol).

-include_lib("plain_fsm/include/plain_fsm.hrl").
-include("include/cap.hrl").

%% API
-export([start_link/4]).

%% Callbacks
-export([data_vsn/0,
         code_change/3]).

%%
%% API
%%


-spec start_link(pid(), inet:socket(), cowboy_tcp_transport, listener())
                -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(_Pid, _Client, cowboy_tcp_transport, _Config) ->
    {error, not_implemented}.

%%
%% Callbacks
%%

code_change(_OldVsn, _State, _Extra) -> {ok, {newstate, data_vsn()}}.

data_vsn() -> 1.
