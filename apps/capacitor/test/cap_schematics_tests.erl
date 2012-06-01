%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(cap_schematics_tests).

-include("include/cap_test.hrl").

%%
%% Fixtures
%%

%% ack=true causes the next payload to be returned

%% ack=false causes the same payload to be returned

%% sequence of messages written to a topic
%% are read from a bound queue in the correct order

%% message written to a topic cannot be
%% read from a queue bound to a different topic

%% bound queues are deleted when a topic is deleted

%% topics are not deleted when a queue is deleted

%% creating a new queue causes the related topic to be created

%% creating a topic doesn't cause any queues to be created

%% there are no messages contained in a new queue
%% bound to a new topic

%% there are no messages contained in a new queue
%% for an existing topic
