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

-record(s, {sup      :: pid(),
            sock     :: inet:socket(),
            session  :: string()}).

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
handle_cast(Frame = #stomp_frame{command = Cmd}, State) ->
    case Cmd of
        "STOMP"       -> connect(Frame, State);
        "CONNECT"     -> connect(Frame, State);
        "SEND"        -> send(Frame, State);
        "SUBSCRIBE"   -> subscribe(Frame, State);
        "UNSUBSCRIBE" -> unsubscribe(Frame, State);
        "ACK"         -> ack(Frame, State);
        "NACK"        -> nack(Frame, State);
        "DISCONNECT"  -> disconnect(State);
        _Unknown      -> unsupported(State)
    end;
handle_cast(stop, State) ->
    {stop, normal, State};
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
%% Handlers
%%

%% @private
connect(_Frame, State = #s{session = undefined}) ->
    Session = riak_core_util:unique_id_62(),
    Headers = [{"session", Session},
               ?SERVER_HEADER,
               ?VERSION_HEADER],
    send_frame("CONNECTED", Headers, "", State#s{session = Session});
connect(_Frame, State) ->
    send_error(already_connected, State).

%% @private
send(Frame = #stomp_frame{command = _Cmd, headers = _Headers, body = _Body},
     State) ->
    %% Check destination header exists
    case restomp:header(Frame, "destination") of
        {ok, Dest} ->
            case split_destination(Dest) of
                {topic, _Topic}  -> {noreply, State};
                _Other           -> send_error(invalid_destination, State)
            end;
        not_found ->
            send_error(no_destination, State)
    end.

%% @private
subscribe(Frame, State) ->
    Fun = fun(H) -> restomp:header(Frame, H) end,
    case {Fun("ack"), Fun("id"), Fun("destination")} of
        {{ok, "client"}, {ok, _Id}, {ok, Dest}} ->
            case split_destination(Dest) of
                {queue, _Topic, _Queue} -> {noreply, State};
                _Other                  -> send_error(invalid_destination, State)
            end;
        {_Ack, not_found, _Dest} ->
            send_error(no_id, State);
        {_Ack, _Id, _Dest} ->
            send_error(must_set_ack_client, State)
    end.

%% @private
unsubscribe(Frame, State) ->
    case restomp:header(Frame, "id") of
        {ok, _Id} ->
            {noreply, State};
        not_found ->
            send_error(no_id, State)
    end.

%% @private
ack(Frame, State) ->
    Fun = fun(H) -> restomp:header(Frame, H) end,
    case {Fun("message-id"), Fun("subscription")} of
        {{ok, _Id}, {ok, _Sub}} ->
            {noreply, State};
        {not_found, _Sub} ->
            send_error(no_message_id, State);
        {_Id, not_found} ->
            send_error(no_subscription, State)
    end.

%% @private
nack(Frame, State) ->
    Fun = fun(H) -> restomp:header(Frame, H) end,
    case {Fun("message-id"), Fun("subscription")} of
        {{ok, _Id}, {ok, _Sub}} ->
            {noreply, State};
        {not_found, _Sub} ->
            send_error(no_message_id, State);
        {_Id, not_found} ->
            send_error(no_subscription, State)
    end.

%% @private
disconnect(State) -> {stop, normal, State}.

%% @private
unsupported(State) -> send_error(unsupported_command, State).

%%
%% Validation
%%

%% @private
split_destination(Dest) ->
    case re:split(Dest, "/") of
        [<<>>, <<>>, <<>>]     -> invalid;
        [<<>>, _Topic, <<>>]   -> invalid;
        [<<>>, Topic]          -> {topic, Topic};
        [<<>>, Topic, Queue]
          when size(Queue) > 0 -> {queue, Topic, Queue};
        _Other                 -> invalid
    end.

%%
%% Communication
%%

%% @private
send_error(Message, State) ->
    Headers = [{"message", atom_to_list(Message)},
               ?CONTENT_TEXT,
               ?VERSION_HEADER],
    case send_frame("ERROR", Headers, [], State) of
        {noreply, NewState} -> {stop, Message, NewState};
        Error               -> Error
    end.

%% @private
send_frame(Command, Headers, Body, State) ->
    send_frame(#stomp_frame{command = Command, headers = Headers, body = Body}, State).

-spec send_frame(restomp:frame(), inet:socket()) -> #s{}.
%% @private
send_frame(Frame, State = #s{sock = Sock}) ->
    case gen_tcp:send(Sock, restomp:encode(Frame)) of
        ok    -> {noreply, State};
        Error -> {stop, Error, State}
    end.
