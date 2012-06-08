%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(railgun_publisher_sup).

-behavior(supervisor).

%% API
-export([start_link/0,
         start_publisher/1]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_publisher(Args) -> supervisor:start_child(?MODULE, Args).

%%
%% Callbacks
%%

init([]) ->
    Spec = {undefined,
            {railgun_publisher, start_link, []},
            temporary, 5000, worker, [railgun_publisher]},
    {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.
