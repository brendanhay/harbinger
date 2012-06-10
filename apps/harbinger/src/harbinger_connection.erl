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
-export([start_link/3]).

%% Callbacks
-export([init/3]).

%%
%% Types
%%

-record(s, {sup                    :: pid(),
            processor              :: pid(),
            sock                   :: inet:socket(),
            parser = restomp:new() :: restomp:parser()}).

%%
%% API
%%

-spec start_link(pid(), pid(), inet:socket()) -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(SupPid, Processor, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [SupPid, Processor, Sock])}.

%%
%% Callbacks
%%

-spec init(pid(), pid(), inet:socket()) -> ok.
%% @hidden
init(SupPid, Processor, Sock) ->
    recv(#s{sup = SupPid, processor = Processor, sock = Sock}).

%%
%% States
%%

recv(State = #s{sock = Sock}) ->
    case async_recv(Sock) of
        {ok, Data} -> parse(Data, State);
        stop       -> lager:info("Closing STOMP connection ~p", [self()])
    end.

parse(Data, State = #s{processor = Processor, parser = Parser}) ->
    case restomp:decode(Data, Parser) of
        {more, NewParser} ->
            recv(State#s{parser = NewParser});
        {ok, Frame, Rest} ->
            ok = harbinger_processor:process(Processor, Frame),
            parse(Rest, #s{parser = restomp:new()})
    end.

%%
%% Private
%%

-spec async_recv(inet:socket()) -> {ok, binary()} | stop.
%% @private
async_recv(Sock) -> async_recv(Sock, 0, infinity).

-spec async_recv(inet:socket(), integer(), pos_integer() | infinity)
                -> {ok, binary()} | stop.
%% @private
async_recv(Sock, Length, Timeout) ->
    ok = prim_inet:async_recv(Sock, Length, Timeout),
    receive
        {inet_async, Sock, _Ref, {ok, Data}} ->
            {ok, Data};
        {inet_async, _Sock, _Ref, {error, closed}} ->
            stop;
        {inet_async, _Sock, _Ref, {error, Reason}} ->
            error({connection_error, Reason})
    end.
