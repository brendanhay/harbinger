%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(cap_queue).

-behaviour(gen_server).

-include("include/cap.hrl").

%% API
-export([start_link/1,
         read/2]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {topic :: binary(),
            path  :: string(),
            ref   :: file:io_device()}).

-define(REG(Name), {via, gproc, {n, l, Name}}).

%%
%% API
%%

-spec start_link(binary()) -> {ok, pid()} | {error, _} | ignore.
%% @doc
start_link(Queue) ->
    lager:info("QUEUE-START ~s", [Queue]),
    gen_server:start_link(?REG(Queue), ?MODULE, Queue, []).

-spec read(binary()) -> #payload{}.
%% @doc
read(Queue) -> gen_server:call(?REG(Queue), read).

%%
%% Callbacks
%%

-spec init(binary()) -> {ok, #s{}}.
%% @hidden
init(Name) ->
    process_flag(trap_exit, true),
    Path = filename:join([code:lib_dir(capacitor, priv),
                          "topics",
                          binary_to_list(Name) ++ ".topic"]),
    {ok, Ref} = file:open(Path, [raw, append, binary]),
    lager:info("TOPIC-LOG ~s", [Path]),
    {ok, #s{name = Name, path = Path, ref = Ref}}.

-spec handle_call(_, {pid(), _}, #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call({append, Bin}, _From, State = #s{ref = Ref}) ->
    {reply, file:write(Ref, Bin), State}.

-spec handle_cast(_, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

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
