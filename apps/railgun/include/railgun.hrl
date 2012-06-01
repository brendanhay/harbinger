%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

%%
%% Parse Transforms
%%

%% Logging
-compile({parse_transform, lager_transform}).

%% Currying
-compile({parse_transform, cut}).

%% Monads
-compile({parse_transform, do}).

%%
%% Types
%%

-type options()  :: [proplists:property()].

-type listener() :: [{ip, string()} |
                     {port, pos_integer()} |
                     {max, pos_integer()}].

%%
%% Monads
%%

-type error_m(Result, Error) :: ok | {ok, Result} | {error, Error}.
-type truth_m() :: true | false.

%%
%% Records
%%

%% -include("railgun_pb.hrl").

%%
%% GProc
%%

-define(AGGR(Name), {a, l, {?MODULE, Name}}).
-define(CNTR(Name), {c, l, {?MODULE, Name}}).
-define(PROP(Name), {p, l, {?MODULE, Name}}).

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
