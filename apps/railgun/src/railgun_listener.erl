%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(railgun_listener).

-include("railgun.hrl").

%% API
-export([start_link/0]).

%%
%% API
%%

-spec start_link() -> {ok, pid()}.
%% @doc
start_link() ->
    case cowboy:start_listener(
           tcp_listener, 100,
           cowboy_tcp_transport, [{port, 5600}],
           railgun_connection, []) of
        {ok, Pid} ->
            lager:info("Listening on 0.0.0.0:5600"),
            {ok, Pid};
        Error ->
            lager:error("Listener failed to start ~p", [Error]),
            error({start_listener_failure, Error})
    end.
