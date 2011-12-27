%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Zotonic statistics application

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

-module(statz).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(application).

-export([
    % application start/stop
    start/0,
    start/2,
    stop/1,

    % create/delete statz servers
    new/1,
    update/2,
    incr/1,
    msec/2,
    msec/3,
    summary/1,
    delete/1,
    
    % Query for stats
    match/1
]).


start() ->
    check(),
    application:start(statz).

start(_StartType, _StartArgs) ->
    case statz_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end.
stop(_State) ->
    ok.



new(Name) ->
    statz_register:new(Name).

update(Value, Name) ->
    statz_register:update(Value, Name).

incr(Name) ->
    statz_register:incr(Name).

msec(Start, Name) ->
    msec(Start, now(), Name).

msec(Start, End, Name) ->
    statz_register:update(now_to_msec(End) - now_to_msec(Start), Name).

now_to_msec({Mega, Sec, Micro}) ->
    Mega * 1000000000 + Sec * 1000 + Micro div 1000.


summary(Name) ->
    statz_register:summary(Name).

delete(Name) ->
    statz_register:delete(Name).

match(Pattern) ->
    statz_register:match(Pattern).

-spec check() -> ok.
check() ->
    ok.


