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
-export([start_link/1]).

%%
%% API
%%

-spec start_link(inet:port_number()) -> {ok, pid()}.
%% @doc
start_link(Port) ->
    case cowboy:start_listener(
           tcp_listener, 100,
           cowboy_tcp_transport, [{port, Port}],
           railgun_connection_sup, []) of
        {ok, Pid} ->
            lager:info("Listening on 0.0.0.0:~p", [Port]),
            {ok, Pid};
        Error ->
            lager:error("Listener failed to start ~p", [Error]),
            error({start_listener_failure, Error})
    end.
