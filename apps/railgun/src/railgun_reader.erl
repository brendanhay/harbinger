%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(railgun_reader).

%% -behaviour(gen_fsm).
%% -behaviour(cowboy_protocol).

%% -include("railgun.hrl").

%% %% API
%% -export([start_link/4]).

%% %% Callbacks
%% -export([init/1,
%%          handle_event/3,
%%          handle_sync_event/4,
%%          handle_info/3,
%%          terminate/3,
%%          code_change/4]).

%% %% States
%% -export([handshaking/2,
%%          active/2]).

%% -record(s, {sock :: inet:socket()}).

%% %%
%% %% API
%% %%

%% -spec start_link(pid(), inet:socket()) -> {ok, pid()} | ignore | {error, _}.
%% %% @doc
%% start_link(Processor, Sock) ->
%%     gen_fsm:start_link(?MODULE, {Processor, Sock}, []).

%% %%
%% %% Callbacks
%% %%

%% -spec init({pid(), inet:socket()}) -> {ok, accepting, #s{}}.
%% %% @hidden
%% init({Processor, Sock}) ->
%%     process_flag(trap_exit, true),
%%     {ok, accepting, #s{processor = Processor, sock = Sock}}.

%% %% @hidden
%% handle_event(_Event, StateName, State) ->
%%     {next_state, StateName, State}.

%% %% @hidden
%% handle_sync_event(_Event, _From, StateName, State) ->
%%     {reply, ok, StateName, State}.

%% %% @hidden
%% handle_info({shoot, _Listener}, accepting, State) ->
%%     {next_state, handshaking, State};
%% handle_info(_Msg, StateName, State) ->
%%     {next_state, StateName, State}.

%% %% @hidden
%% terminate(_Reason, _StateName, State) ->
%%     close(State).

%% %% @hidden
%% code_change(_OldVsn, StateName, State, _Extra) ->
%%     {ok, StateName, State}.

%% %%
%% %% States
%% %%

%% %% Connecting
%% %%
%% %% A STOMP client initiates the stream or TCP connection to the server
%% %% by sending the CONNECT frame:

%% %% CONNECT
%% %% accept-version:1.1
%% %% host:stomp.github.org
%% %%
%% %% ^@

%% %% If the server accepts the connection attempt it will
%% %% respond with a CONNECTED frame:

%% %% CONNECTED
%% %% version:1.1
%% %%
%% %% ^@

%% %% The server can reject any connection attempt.
%% %% The server SHOULD respond back with an ERROR frame listing
%% %% why the connection was rejected and then close the connection.
%% %% STOMP servers MUST support clients which rapidly connect and disconnect.
%% %% This implies a server will likely only allow closed connections to
%% %% linger for short time before the connection is reset.
%% %% This means that a client may not receive the ERROR frame
%% %% before the socket is reset.

%% handshaking(_Event, State = #s{sock = Sock}) ->
%%     ok = inet:setopts(Sock, [{active, once}]),
%%     {next_state, recv, State}.

%% recv(_Event, State) ->
%%     {next_state, recv, State};
%% recv(_Event, State) ->
%%     {next_state, more, State}.

%% more(_Event, State) ->
%%     {next_state, recv, State};
%% more(_Event, State) ->
%%     {next_state, more, State}.

%% %%
%% %% Private
%% %%

%% -spec send(inet:socket(), iolist()) -> ok.
%% %% @private
%% send(Sock, Data) ->
%%     case gen_tcp:send(Sock, Data) of
%%         ok    -> ok;
%%         Error -> lager:debug("CONN-CLOSED ~p - ~p", [Error, Data])
%%     end.

%% -spec close(#s{}) -> ok.
%% %% @private
%% close(#s{sock = Sock}) ->
%%     ok = send(Sock, <<"ERROR">>),
%%     gen_tcp:close(Sock).
