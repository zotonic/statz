%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Supervisor for the statz application, supervises all stats servers

%% Copyright 2011 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% This code is based on the algorithm and proof-of-concept by Daniel Nichter
%% http://hackmysql.com/blog/2011/08/29/new-algorithm-for-calculating-95-percentile/
%%
%% The 95th or 99th percentile are estimated using running statistics of the numbers seen.
%% The estimates are in practice good enough to be trusted.
%% This code only needs to maintain counters for a sample of 100-150 numbers, compared to
%% all data for the usual algorithms.

%% Based on mochiweb_sup.

-module(statz_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

upgrade() ->
    {ok, {_, Specs}} = init([]),
    lists:foreach(fun(S) -> supervisor:start_child(?MODULE, S) end, Specs),
    ok.

init([]) ->
    Processes = [
        {statz_register, {statz_register, start_link, []},
            permanent, 2000, worker, [statz_register]}
    ],
    {ok, {{one_for_all, 10, 10}, Processes}}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
