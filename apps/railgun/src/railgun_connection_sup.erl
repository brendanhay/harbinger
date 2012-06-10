%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc Used by the `harbinger-admin` script
%%

-module(harbinger_connection_sup).

-behaviour(supervisor2).

%% API
-export([start_link/4]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link(pid(), inet:socket(), cowboy_tcp_transport, []) ->
                        {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(_Listener, Sock, cowboy_tcp_transport, []) ->
    %% We want the connection to be transient since when it exits normally
    %% the processor may have some work still to do (and the connection
    %% tells the processor to exit). However, if the connection terminates
    %% abnormally then we want to take everything down.
    %%
    %% The *processor* however is intrinsic, so when it exits, the
    %% supervisor2 goes too.
    {ok, SupPid}        = supervisor2:start_link(?MODULE, []),
    {ok, ProcessorPid}  = start_processor(SupPid, Sock),
    {ok, ConnectionPid} = start_connection(SupPid, ProcessorPid, Sock),
    %% ok                  = gen_tcp:controlling_process(Sock, ConnectionPid),
    {ok, SupPid}.

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 0, 1}, [supervisor2:child_spec()]}}.
%% @hidden
init([]) -> {ok, {{one_for_all, 0, 1}, []}}.

%%
%% Private
%%

-spec start_processor(pid(), inet:socket()) -> {ok, pid()}.
%% @private
start_processor(SupPid, Sock) ->
    Spec = {harbinger_processor,
            {harbinger_processor, start_link, [SupPid, Sock]},
            intrinsic, 5000, worker, [harbinger_processor]},
    supervisor2:start_child(SupPid, Spec).

-spec start_connection(pid(), pid(), inet:socket()) -> {ok, pid()}.
%% @private
start_connection(SupPid, ProcessorPid, Sock) ->
    Spec = {harbinger_connection,
            {harbinger_connection, start_link, [SupPid, ProcessorPid, Sock]},
            transient, 2000, worker, [harbinger_connection]},
    supervisor2:start_child(SupPid, Spec).
