%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(railgun_ring_event_handler).

-behaviour(gen_event).

-include("railgun.hrl").

%% Callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%
%% Types
%%

-record(s, {}).

%%
%% Callbacks
%%

init([]) ->
    {ok, #s{}}.

handle_event({ring_update, Ring}, State) ->
    lager:info("Update Event ~p ~p", [Ring, State]),
    {ok, State}.

handle_call(Event, State) ->
    lager:info("Event Call ~p", [Event]),
    {ok, ok, State}.

handle_info(Info, State) ->
    lager:info("Event Info ~p", [Info]),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

