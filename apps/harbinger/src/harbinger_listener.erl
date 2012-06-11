%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_listener).

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
%% Types
%%

-record(s, {}).

%%
%% API
%%

-spec start_link(string(), inet:port_number())
                -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(IpAddr, Port) -> gen_nb_server:start_link(?MODULE, IpAddr, Port, []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, #s{}}.
%% @hidden
init([]) -> {ok, #s{}}.

-spec handle_call(_, {pid(), _}, #s{}) -> {stop, {unhandled_call, _}, #s{}}.
%% @hidden
handle_call(Msg, _From, State) -> {stop, {unhandled_call, Msg}, State}.

-spec handle_cast(_, #s{}) -> {stop, {unhandled_cast, _}, #s{}}.
%% @hidden
handle_cast(Msg, State) -> {stop, {unhandled_cast, Msg}, State}.

-spec handle_info(_, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec sock_opts() -> [gen_tcp:option()].
%% @hidden
sock_opts() -> [binary, {active, once}, {packet, 0}, {linger, {false, 0}}].

-spec new_connection(inet:socket(), #s{}) -> {ok, #s{}}.
%% @hidden
new_connection(Sock, State) ->
    {ok, _SupPid, ReaderPid} = harbinger_connection_sup:start_child(Sock),
    ok = harbinger_reader:handoff_socket(ReaderPid, Sock),
    {ok, State}.
