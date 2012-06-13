%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_connection_sup).

-behaviour(supervisor2).

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link(inet:socket()) -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(Sock) ->
    %% We want the connection to be transient since when it exits normally
    %% the processor may have some work still to do (and the connection
    %% tells the processor to exit). However, if the connection terminates
    %% abnormally then we want to take everything down.
    %%
    %% The *processor* however is intrinsic, so when it exits, the
    %% supervisor2 goes too.
    {ok, SupPid}    = supervisor2:start_link(?MODULE, []),
    {ok, ProcPid}   = start_processor(SupPid, Sock),
    {ok, ReaderPid} = start_reader(SupPid, ProcPid, Sock),
    {ok, SupPid, ReaderPid}.

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

-spec start_reader(pid(), pid(), inet:socket()) -> {ok, pid()}.
%% @private
start_reader(SupPid, ProcPid, Sock) ->
    Spec = {harbinger_reader,
            {harbinger_reader, start_link, [SupPid, ProcPid, Sock]},
            transient, 2000, worker, [harbinger_reader]},
    supervisor2:start_child(SupPid, Spec).
