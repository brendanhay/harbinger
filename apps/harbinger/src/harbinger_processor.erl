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

-include("restomp.hrl").
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

-record(s, {sup     :: pid(),
            sock    :: inet:socket(),
            session :: string()}).

-define(VERSIONS,       ["1.0", "1.1"]).
-define(VERSION_HEADER, {"version", string:join(?VERSIONS, ",")}).
-define(SERVER_HEADER,  {"server", "Harbinger/0.1.0"}).
-define(CONTENT_TEXT,   {"content-type", "text/plain"}).

%%
%% API
%%

-spec start_link(pid(), inet:socket()) -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(SupPid, Sock) -> gen_server:start_link(?MODULE, {SupPid, Sock}, []).

-spec process(pid(), #stomp_frame{}) -> ok.
%% @doc
process(Pid, Frame) -> gen_server:cast(Pid, Frame).

-spec stop(pid()) -> ok.
%% @doc
stop(Pid) -> gen_server:cast(Pid, stop).

%%
%% Callbacks
%%

-spec init({pid(), inet:socket()}) -> {ok, #s{}}.
%% @hidden
init({SupPid, Sock}) ->
    process_flag(trap_exit, true),
    {ok, #s{sup = SupPid, sock = Sock}}.

-spec handle_call(_, {pid(), _}, #s{}) -> {stop, {unhandled_call, _}, #s{}}.
%% @hidden
handle_call(Msg, _From, State) -> {stop, {unhandled_call, Msg}, State}.

-spec handle_cast(stop | _, #s{}) -> {stop, normal, #s{}}.
%% @hidden
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Frame = #stomp_frame{command = Cmd}, State) ->
    case Cmd of
        "STOMP"      -> connect(Frame, State);
        "CONNECT"    -> connect(Frame, State);
        "DISCONNECT" -> disconnect(Frame, State);
        _Unknown     -> unsupported(Frame, State)
    end;
handle_cast(Msg, State) ->
    {stop, {unhandled_cast, Msg}, State}.

-spec handle_info(_, #s{}) -> {stop, {unhandled_call, _}, #s{}}.
%% @hidden
handle_info(Info, State) -> {stop, {unhandled_info, Info}, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Frame Handlers
%%

%% @private
connect(#stomp_frame{}, State = #s{session = undefined}) ->
    Session = riak_core_util:unique_id_62(),
    Headers = [{"session", Session},
               ?SERVER_HEADER,
               ?VERSION_HEADER],
    send_frame("CONNECTED", Headers, "", State#s{session = Session});
connect(#stomp_frame{command = Cmd}, State = #s{session = Session}) ->
    send_error(already_connected,
               "Issued '~s' command when '~s' already connected.~n",
               [Cmd, Session],
               State).

%% @disconnect
disconnect(_Frame, State) -> {stop, normal, State}.

%% @private
unsupported(#stomp_frame{command = Cmd}, State) ->
    send_error(unsupported_command,
               "Command '~p' not supported.~n",
               [Cmd],
               State).

%%
%% Socket Communication
%%

%% @private
send_error(Message, Format, Args, State) ->
    send_error(Message, lists:flatten(io_lib:format(Format, Args)), State).

%% @private
send_error(Message, Detail, State) ->
    Headers = [{"message", atom_to_list(Message)},
               ?CONTENT_TEXT,
               ?VERSION_HEADER],
    case send_frame("ERROR", Headers, Detail, State) of
        {noreply, NewState} -> {stop, Message, NewState};
        Error               -> Error
    end.

%% @private
send_frame(Command, Headers, Body, State) ->
    send(#stomp_frame{command = Command, headers = Headers, body = Body}, State).

-spec send(restomp:frame(), inet:socket()) -> #s{}.
%% @private
send(Frame, State = #s{sock = Sock}) ->
    case gen_tcp:send(Sock, restomp:encode(Frame)) of
        ok    -> {noreply, State};
        Error -> {stop, Error, State}
    end.
