%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc Used by the `railgun-admin` script
%%

-module(railgun_connection_sup).

%% -behaviour(supervisor2).

%% %% API
%% -export([start_link/0]).

%% %% Callbacks
%% -export([init/1]).

%% %%
%% %% API
%% %%

%% -spec start_link(pid(), inet:socket(), cowboy_tcp_transport, []) ->
%%                         {ok, pid()} | ignore | {error, _}.
%% %% @doc
%% start_link(_Listener, Sock, cowboy_tcp_transport, []) ->
%%     {ok, SupPid}       = supervisor2:start_link(?MODULE, []),
%%     {ok, ProcessorPid} = start_processor(SupPid, Sock),
%%     {ok, _ReaderPid}   = start_reader(SupPid, ProcessorPid, Sock),
%%     {ok, SupPid}.

%% %%
%% %% Callbacks
%% %%

%% -spec init([]) -> {ok, {{one_for_all, 0, 1}, [supervisor:child_spec()]}}.
%% %% @hidden
%% init([]) -> {ok, {{one_for_all, 0, 1}, []}}.

%% %%
%% %% Private
%% %%

%% -spec start_processor(pid(), inet:socket()) -> {ok, pid()}.
%% %% @private
%% start_processor(SupPid, Sock) ->
%%     Spec = {railgun_processor,
%%             {railgun_processor, start_link, [Sock]},
%%             permanent, 5000, worker, [railgun_processor]},
%%     supervisor2:start_child(SupPid, Spec).

%% -spec start_reader(pid(), inet:socket()) -> {ok, pid()}.
%% %% @private
%% start_reader(SupPid, ProcessorPid, Sock) ->
%%     Spec = {railgun_reader,
%%             {railgun_reader, start_link, [ProcessorPid, Sock]},
%%             transient, 5000, worker, [railgun_reader]},
%%     supervisor2:start_child(SupPid, Spec).
