%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Statistics server.

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

-module(statz_server).
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

-export([
    update/2,
    incr/1,
    delete/1,
    summary/1
]).

-record(state, {
    is_value,
    perc95,
    perc99,
    sample,
    spiral
}).


%%====================================================================
%% API
%%====================================================================

update(Value, Pid) when is_integer(Value), Value >= 0 ->
    gen_server:cast(Pid, {update, Value});
update(_Value, _Pid) ->
    {error, illegal_value}.
    

incr(Pid) ->
    gen_server:cast(Pid, incr).

delete(Pid) ->
    gen_server:cast(Pid, stop).

summary(Pid) ->
    gen_server:call(Pid, summary).


start_link() ->
    start_link([]).
start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{
        is_value = false,
        perc95 = percentile:new(95),
        perc99 = percentile:new(99),
        spiral = spiraltime:fresh(),
        sample = basho_stats_sample:new()
    }}.



handle_call(summary, _From, State) ->
    {Secs, Mins, Hours, Days} = spiraltime:totals(State#state.spiral),
    Counters = [
        {second, Secs},
        {minute, Mins},
        {hour, Hours},
        {day, Days}
    ],
    
    case State#state.is_value of
        true ->
            {Min, Mean, Max, Variance, Sdev} = basho_stats_sample:summary(State#state.sample),
            Reply = [
                {perc95, percentile:percentile(State#state.perc95)},
                {perc99, percentile:percentile(State#state.perc99)},

                {min, Min},
                {avg, Mean},
                {max, Max},
                {variance, Variance},
                {sdev, Sdev}
                | Counters
            ];
        false ->
            Reply = Counters
    end,
    {reply, {ok, Reply}, State};
handle_call(Msg, _From, State) ->
    {stop, {uknown_call, Msg}, State}.


handle_cast({update, Value}, State) ->
    {noreply, State#state{
        is_value = true,
        perc95 = percentile:update(Value, State#state.perc95),
        perc99 = percentile:update(Value, State#state.perc99),
        spiral = spiraltime:incr(Value, State#state.spiral),
        sample = basho_stats_sample:update(Value, State#state.sample)
    }};
handle_cast(incr, State) ->
    {noreply, State#state{spiral = spiraltime:incr(1, State#state.spiral)}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.
 
 
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


