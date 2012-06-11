
%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%
%% This code has been adapted from the RabbitMQ STOMP plugin:
%% http://hg.rabbitmq.com/rabbitmq-stomp
%%

-module(restomp).

-include("restomp.hrl").
-include("harbinger.hrl").

%% API
-export([new/0,
         encode/1,
         encode/3,
         decode/1,
         decode/2,
         header/2,
         header/3,
         boolean_header/2,
         boolean_header/3,
         integer_header/2,
         integer_header/3,
         binary_header/2,
         binary_header/3]).

%%
%% Types
%%

-opaque stomp_frame() :: #stomp_frame{}.

-type command() :: string().
-type headers() :: [proplists:property()].
-type body()    :: [binary()].
-type parser()  :: none | {resume, fun((binary()) -> ok)}.

-exported_types([stomp_frame/0,
                 parser/0]).

%%
%% API
%%

-spec new() -> parser().
new() -> none.

-spec encode(stomp_frame()) -> binary().
%% @doc
encode(#stomp_frame{command = Cmd, headers = Headers, body = Body}) ->
    encode(Cmd, Headers, Body).

-spec encode(command(), headers(), body()) -> binary().
%% @doc
encode(Cmd, Headers, Body) ->
    Len = iolist_size(Body),
    Res = [Cmd, $\n,
           lists:map(fun serialize_header/1,
                     lists:reverse(lists:keydelete("content-length", 1, Headers))),
           if
               Len > 0 -> ["content-length:", integer_to_list(Len), $\n];
               true    -> []
           end,
           $\n, Body, 0],
    iolist_to_binary(Res).

-spec decode(binary()) -> parser().
%% @doc
decode(Bin) -> decode(Bin, none).

-spec decode(binary(), parser()) -> parser().
%% @doc
decode(Bin, {resume, Fun}) -> Fun(Bin);
decode(Bin, none)          -> parse_command(Bin, []).

%% @doc
header(#stomp_frame{headers = Headers}, Key) ->
    case lists:keysearch(Key, 1, Headers) of
        {value, {_, Str}} -> {ok, Str};
        _                 -> not_found
    end.

%% @doc
header(F, K, D) -> default_value(header(F, K), D).

%% @doc
boolean_header(#stomp_frame{headers = Headers}, Key) ->
    case lists:keysearch(Key, 1, Headers) of
        {value, {_, "true"}}  -> {ok, true};
        {value, {_, "false"}} -> {ok, false};
        _                     -> not_found
    end.

%% @doc
boolean_header(F, K, D) -> default_value(boolean_header(F, K), D).

%% @doc
internal_integer_header(Headers, Key) ->
    case lists:keysearch(Key, 1, Headers) of
        {value, {_, Str}} -> {ok, list_to_integer(string:strip(Str))};
        _                 -> not_found
    end.

%% @doc
integer_header(#stomp_frame{headers = Headers}, Key) ->
    internal_integer_header(Headers, Key).

%% @doc
integer_header(F, K, D) -> default_value(integer_header(F, K), D).

%% @doc
binary_header(F, K) ->
    case header(F, K) of
        {ok, Str} -> {ok, list_to_binary(Str)};
        not_found -> not_found
    end.

%% @doc
binary_header(F, K, D) -> default_value(binary_header(F, K), D).

%%
%% Private
%%

parse_command(<<>>, Acc) ->
    more(fun(Rest) -> parse_command(Rest, Acc) end);
parse_command(<<$\n, Rest/binary>>, []) ->  % inter-frame newline
    parse_command(Rest, []);
parse_command(<<$\r, $\n, Rest/binary>>, []) ->  % inter-frame newline
    parse_command(Rest, []);
parse_command(<<0, Rest/binary>>, []) ->    % empty frame
    parse_command(Rest, []);
parse_command(<<$\n, Rest/binary>>, Acc) -> % end command
    parse_headers(Rest, lists:reverse(Acc));
parse_command(<<$\r, $\n, Rest/binary>>, Acc) -> % end command
    parse_headers(Rest, lists:reverse(Acc));
parse_command(<<Ch:8, Rest/binary>>, Acc) ->
    parse_command(Rest, [Ch|Acc]).

parse_headers(Rest, Command) -> % begin headers
    parse_headers(Rest, #stomp_frame{command = Command}, [], []).

parse_headers(<<>>, Frame, HeaderAcc, KeyAcc) ->
    more(fun(Rest) -> parse_headers(Rest, Frame, HeaderAcc, KeyAcc) end);
parse_headers(<<$\n, Rest/binary>>, Frame, HeaderAcc, _KeyAcc) -> % end headers
    parse_body(Rest, Frame#stomp_frame{headers = HeaderAcc});
parse_headers(<<$\r, $\n, Rest/binary>>, Frame, HeaderAcc, _KeyAcc) -> % end headers
    parse_body(Rest, Frame#stomp_frame{headers = HeaderAcc});
parse_headers(<<$:, Rest/binary>>, Frame, HeaderAcc, KeyAcc) ->   % end key
    parse_header_value(Rest, Frame, HeaderAcc, lists:reverse(KeyAcc));
parse_headers(<<Ch:8, Rest/binary>>, Frame, HeaderAcc, KeyAcc) ->
    parse_headers(Rest, Frame, HeaderAcc, [Ch|KeyAcc]).

parse_header_value(Rest, Frame, HeaderAcc, Key) -> % begin header value
    parse_header_value(Rest, Frame, HeaderAcc, Key, []).

parse_header_value(<<>>, Frame, HeaderAcc, Key, ValAcc) ->
    more(fun(Rest) -> parse_header_value(Rest, Frame, HeaderAcc, Key, ValAcc)
         end);
parse_header_value(<<$\n, Rest/binary>>, Frame, HeaderAcc, Key, ValAcc) -> % end value
    parse_headers(Rest, Frame,
                  insert_header(HeaderAcc, Key, lists:reverse(ValAcc)),
                  []);
parse_header_value(<<$\r, $\n, Rest/binary>>, Frame, HeaderAcc, Key, ValAcc) -> % end value
    parse_headers(Rest, Frame,
                  insert_header(HeaderAcc, Key, lists:reverse(ValAcc)),
                  []);
parse_header_value(<<$\\, Rest/binary>>, Frame, HeaderAcc, Key, ValAcc) ->
    parse_header_value_escape(Rest, Frame, HeaderAcc, Key, ValAcc);
parse_header_value(<<Ch:8, Rest/binary>>, Frame, HeaderAcc, Key, ValAcc) ->
    parse_header_value(Rest, Frame, HeaderAcc, Key, [Ch | ValAcc]).

parse_header_value_escape(<<>>, Frame, HeaderAcc, Key, ValAcc) ->
    more(fun(Rest) ->
           parse_header_value_escape(Rest, Frame, HeaderAcc, Key, ValAcc)
         end);
parse_header_value_escape(<<Ch:8,  Rest/binary>>, Frame,
                          HeaderAcc, Key, ValAcc) ->
    case unescape(Ch) of
        {ok, EscCh} -> parse_header_value(Rest, Frame, HeaderAcc, Key,
                                          [EscCh | ValAcc]);
        error       -> {error, {bad_escape, Ch}}
    end.

insert_header(Headers, Key, Value) ->
    case lists:keysearch(Key, 1, Headers) of
        {value, _} -> Headers; % first header only
        false      -> [{Key, Value} | Headers]
    end.

parse_body(Content, Frame) ->
    parse_body(Content, Frame, [],
               integer_header(Frame, "content-length", unknown)).

parse_body(Content, Frame, Chunks, unknown) ->
    parse_body2(Content, Frame, Chunks, case firstnull(Content) of
                                            -1  -> {more, unknown};
                                            Pos -> {done, Pos}
                                        end);
parse_body(Content, Frame, Chunks, Remaining) ->
    Size = byte_size(Content),
    parse_body2(Content, Frame, Chunks, case Remaining >= Size of
                                            true  -> {more, Remaining - Size};
                                            false -> {done, Remaining}
                                        end).

parse_body2(Content, Frame, Chunks, {more, Left}) ->
    Chunks1 = finalize_chunk(Content, Chunks),
    more(fun(Rest) -> parse_body(Rest, Frame, Chunks1, Left) end);
parse_body2(Content, Frame, Chunks, {done, Pos}) ->
    <<Chunk:Pos/binary, 0, Rest/binary>> = Content,
    Body = lists:reverse(finalize_chunk(Chunk, Chunks)),
    {ok, Frame#stomp_frame{body = Body}, Rest}.

finalize_chunk(<<>>,  Chunks) -> Chunks;
finalize_chunk(Chunk, Chunks) -> [Chunk | Chunks].

more(Continuation) -> {more, {resume, Continuation}}.

default_value({ok, Value}, _DefaultValue) -> Value;
default_value(not_found, DefaultValue)    -> DefaultValue.

serialize_header({K, V}) when is_integer(V) ->
    [K, $:, integer_to_list(V), $\n];
serialize_header({K, V}) when is_list(V) ->
    [K, $:, [escape(C) || C <- V], $\n].

unescape($n)  -> {ok, $\n};
unescape($\\) -> {ok, $\\};
unescape($c)  -> {ok, $:};
unescape(_)   -> error.

escape($:)  -> "\\c";
escape($\\) -> "\\\\";
escape($\n) -> "\\n";
escape(C)   -> C.

firstnull(Content) -> firstnull(Content, 0).

firstnull(<<>>,                _N) -> -1;
firstnull(<<0,  _Rest/binary>>, N) -> N;
firstnull(<<_Ch, Rest/binary>>, N) -> firstnull(Rest, N+1).
