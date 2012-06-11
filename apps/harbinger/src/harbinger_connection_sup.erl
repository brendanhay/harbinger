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

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(inet:socket()) -> {ok, pid(), pid()}.
%% @doc
start_child(Sock) -> supervisor:start_child(?MODULE, [Sock]).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{simple_one_for_one, 0, 1}, [supervisor:child_spec()]}}.
%% @hidden
init([]) ->
    Spec = {connection,
            {harbinger_connection, start_link, []},
            temporary, 1000, supervisor, [harbinger_connection]},
    {ok, {{simple_one_for_one, 0, 1}, [Spec]}}.
