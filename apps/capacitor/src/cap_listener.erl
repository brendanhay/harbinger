%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(cap_listener).

-include("include/cap.hrl").

%% API
-export([start_link/0]).

%%
%% API
%%

-spec start_link() -> ok.
%% @doc
start_link() ->
    [link(P) || {ok, P} <- [listener(C) || C <- cap_config:env(listeners)]],
    ok.

%%
%% API
%%

-spec listener(listener()) -> {ok, pid()}.
%% @private
listener(Config) ->
    Tcp = tcp_options(Config),
    Acceptors = cap_config:option(acceptors, Config),
    case cowboy:start_listener(tcp_listener, Acceptors,
                               cowboy_tcp_transport, Tcp,
                               cap_connection, Config) of
        {ok, Pid} ->
            lager:info("LISTEN ~s", [cap_net:format_ip(Tcp)]),
            {ok, Pid};
        Error ->
            lager:error("LISTENER failed to start ~p", [Error]),
            exit(listener_start_failure)
    end.

-spec tcp_options(listener()) -> [proplists:property()].
%% @private
tcp_options(Config) ->
    [{ip, cap_config:option(ip, Config)},
     {port, cap_config:option(port, Config)}|cap_config:env(tcp)].
