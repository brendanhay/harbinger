%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(railgun_connection).

-behaviour(gen_fsm).
-behaviour(cowboy_protocol).

-include("railgun.hrl").

%% API
-export([start_link/4]).

%% Callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% States
-export([handshaking/2,
         handshaking/3,
         proxying/2,
         closing/2]).

-record(s, {}).

%%
%% API
%%

-spec start_link(pid(), inet:socket(), cowboy_tcp_transport, _)
                -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(_Listener, Sock, cowboy_tcp_transport, _Config) ->
    gen_fsm:start_link(?MODULE, Sock, []).

%%
%% Callbacks
%%

-spec init(inet:socket()) -> {ok, accepting, #s{}}.
%% @hidden
init(Sock) ->
    process_flag(trap_exit, true),
    {ok, accepting, #s{sock = Sock}}.

%% @hidden
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @hidden
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

%% @hidden
handle_info(_Msg, _Any, State) ->
    {next_state, closing, State};
handle_info({shoot, Listener}, accepting, State) ->
    {next_state, handshaking, State}.

%% @hidden
terminate(Reason, StateName, State) ->
    close(State).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% Private
%%

-spec send(inet:socket(), iolist()) -> ok.
%% @private
send(Sock, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok    -> ok;
        Error -> lager:debug("CONN-CLOSED ~p - ~p", [Error, Data])
    end.

-spec close_sock(#s{}) -> ok.
%% @private
close(#s{sock = Sock}) ->
    send(Sock, <<"ERROR">>),
    gen_tcp:close(Sock).
