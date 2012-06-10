%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_connection).

-include("harbinger.hrl").

%% API
-export([start_link/3,
         transfer_socket/2]).

%% Callbacks
-export([init/3]).

%%
%% Types
%%

-record(s, {sup                    :: pid(),
            processor              :: pid(),
            sock                   :: inet:socket(),
            parser = restomp:new() :: restomp:parser()}).

-define(TIMEOUT, 2000).

%%
%% API
%%

-spec start_link(pid(), pid(), inet:socket())
                -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(SupPid, ProcPid, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [SupPid, ProcPid, Sock])}.

-spec transfer_socket(pid(), inet:socket()) -> ok.
%% @doc
transfer_socket(Pid, Sock) ->
    ok = gen_tcp:controlling_process(Sock, Pid),
    Pid ! {accepted, Sock},
    ok.

%%
%% Callbacks
%%

-spec init(pid(), pid(), inet:socket()) -> ok.
%% @hidden
init(SupPid, ProcPid, Sock) ->
    receive
        {accepted, Sock} ->
            lager:info("Accepted"),
            recv(#s{sup = SupPid, processor = ProcPid, sock = Sock})
    after
        ?TIMEOUT ->
            error({?MODULE, timeout})
    end.

%%
%% States
%%

recv(State = #s{sock = Sock}) ->
    case async_recv(Sock) of
        {ok, Data} -> parse(Data, State);
        stop       -> lager:info("Closing STOMP connection ~p", [self()])
    end.

parse(Data, State = #s{processor = ProcPid, parser = Parser}) ->
    case restomp:decode(Data, Parser) of
        {more, NewParser} ->
            recv(State#s{parser = NewParser});
        {ok, Frame, Rest} ->
            lager:info("Parsed STOMP frame ~p on ~p", [Frame, self()]),
            ok = harbinger_processor:process(ProcPid, Frame),
            parse(Rest, State#s{parser = restomp:new()})
    end.

%%
%% Private
%%

-spec async_recv(inet:socket()) -> {ok, binary()} | stop.
%% @private
async_recv(Sock) -> async_recv(Sock, 0, -1).

-spec async_recv(inet:socket(), integer(), pos_integer() | infinity)
                -> {ok, binary()} | stop.
%% @private
async_recv(Sock, Length, Timeout) ->
    {ok, Ref} = prim_inet:async_recv(Sock, Length, Timeout),
    receive
        {inet_async, Sock, Ref, {ok, Data}} ->
            {ok, Data};
        {inet_async, Sock, Ref, {error, closed}} ->
            stop;
        {inet_async, Sock, Ref, {error, Reason}} ->
            error({connection_error, Reason})
    end.
