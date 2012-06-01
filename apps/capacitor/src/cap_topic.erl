%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(cap_topic).

-behaviour(gen_server).

-include("include/cap.hrl").

%% API
-export([start_link/1,
         write/2,
         time/2,
         bench/3]).

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
start_link(Topic) ->
    lager:info("TOPIC-START ~s", [Topic]),
    gen_server:start_link(?REG(Topic), ?MODULE, Topic, []).

-spec write(binary(), #payload{} | binary()) -> ok.
%% @doc
write(Name, Payload = #payload{}) ->
    write(Name, cap_pb:encode(Payload));
write(Name, Bin) when is_binary(Bin) ->
    gen_server:call(?REG(Name), {write, Bin}).

time(N, X) ->
    T = list_to_binary("topic." ++ N),
    start_link(T),
    P = cap_pb:encode({payload, <<"application/json">>, <<"text">>}),
    {V, _} = timer:tc(?MODULE, bench, [T, P, X]),
    lager:info("TOPIC-BENCH ~pms for ~p payloads", [(V / 1000), X]).

bench(T, P, X) -> [append(T, P) || _ <- lists:seq(1, X)].

%%
%% Callbacks
%%

-spec init(binary()) -> {ok, #s{}}.
%% @hidden
init(Topic) ->
    process_flag(trap_exit, true),
    Path = filename:join([code:lib_dir(capacitor, priv),
                          "topics",
                          binary_to_list(Topic) ++ ".topic"]),
    {ok, Ref} = file:open(Path, [raw, append, binary]),
    lager:info("TOPIC-LOG ~s", [Path]),
    {ok, #s{topic = Topic, path = Path, ref = Ref}}.

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
