%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(cap_topic_tests).

-include("include/cap_test.hrl").

%%
%% Fixtures
%%

%% sequence of messages written to a topic
%% are read from a bound queue in the correct order

%% message written to a topic cannot be
%% read from a queue bound to a different topic
