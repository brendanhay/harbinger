%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc Used by the `harbinger-admin` script
%%

-module(harbinger_tcp_listener).

-behaviour(gen_nb_server).

-include("harbinger.hrl").

%% API
-export([start_link/2]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         sock_opts/0,
         new_connection/2]).

%%
%% API
%%

start_link(IpAddr, Port) -> gen_nb_server:start_link(?MODULE, IpAddr, Port, []).

%%
%% Callbacks
%%

init([]) -> {ok, []}.

handle_call(Msg, _From, State) -> {stop, {unhandled_call, Msg}, State}.

handle_cast(Msg, State) -> {stop, {unhandled_cast, Msg}, State}.

handle_info(Msg, State) -> {stop, {unhandled_info, Msg}, State}.

terminate(_Reason, _State) -> ok.

sock_opts() -> [binary, {active, once}, {packet, 0}, {linger, {true, 1}}].

new_connection(Sock, State) ->
    {ok, _SupPid} = harbinger_connection_sup:start_link(Sock),
    {ok, State}.
