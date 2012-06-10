%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(harbinger_processor).

-behaviour(gen_server).

-include_lib("restomp/include/restomp.hrl").
-include("harbinger.hrl").

%% API
-export([start_link/2,
         process/2,
         stop/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%%
%% Types
%%

-record(s, {sup :: pid(),
            sock :: inet:socket()}).

-define(VERSIONS, ["1.0", "1.1"]).

%%
%% API
%%

start_link(SupPid, Sock) -> gen_server:start_link(?MODULE, {SupPid, Sock}, []).

process(Pid, Frame) -> gen_server:cast(Pid, Frame).

stop(Pid) -> gen_server:cast(Pid, stop).

%%
%% Callbacks
%%

-spec init({pid(), inet:socket()}) -> {ok, #s{}}.
%% @hidden
init({SupPid, Sock}) ->
    process_flag(trap_exit, true),
    {ok, #s{sup = SupPid, sock = Sock}}.

-spec handle_call(_, {pid(), _}, #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(stop | _, #s{}) -> {stop, normal, #s{}}.
%% @hidden
handle_cast(stop, State) -> {stop, normal, State};

handle_cast(Frame, State)
  when Frame#stomp_frame.command =:= "STOMP"
       orelse Frame#stomp_frame.command =:= "CONNECT" ->
    {noreply, connect(Frame, State)}.

-spec handle_info(_, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

%% @private
connect(_Frame, State) ->
    Headers = [{"session", "0"},
               {"heart-beat", "0,0"},
               {"server", "Railgun/0.1.0"},
               {"version", "1.1"}],
    ok = send_frame("CONNECTED", Headers, "", State),
    State.

%% @private
send_frame(Command, Headers, Body, State) ->
    send(#stomp_frame{command = Command,
                      headers = Headers,
                      body    = Body},
         State).

%% %% @private
%% send_error(Message, Detail, State) ->
%%     send_frame("ERROR", [{"message", Message},
%%                          {"content-type", "text/plain"},
%%                          {"version", string:join(?VERSIONS, ",")}],
%%                          Detail, State).

-spec send(inet:socket(), restomp:frame()) -> ok.
%% @private
send(Sock, Frame) ->
    Data = restomp:encode(Frame),
    case gen_tcp:send(Sock, Data) of
        ok    -> ok;
        Error -> lager:debug("CONN-CLOSED ~p - ~p", [Error, Data])
    end.
