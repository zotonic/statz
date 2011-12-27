%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc 95th/99th Percentile estimator. Estimates the percentile without remembering all values.

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

-module(statz_percentile).

-export([
    new/0,
    new/1,
    update/2,
    percentile/1
]).

-record(state, {
    percentile=0.95,
    above_threshold = 0.04,
    n_events = 0,
    nums = [],
    high_trending = false,
    n_above_max = 0,
    new_nums = []
}).


-define(SAMPLE_SIZE, 100).
-define(TREND_SIZE, 50).

-spec new() -> #state{}.
new() ->
    new(95).


-spec new(pos_integer()) -> #state{}.
new(95) ->
    #state{above_threshold=0.04, percentile=0.95};
new(99) ->
    #state{above_threshold=0.005, percentile=0.99}.


-spec update(integer(), #state{}) -> #state{}.
update(Value, State) ->
    NEvents = State#state.n_events + 1,
    case length(State#state.nums) of
        ?SAMPLE_SIZE ->

            {HighValue,_} = lists:last(State#state.nums),
            HighTrending = State#state.high_trending 
                        orelse (State#state.n_above_max / NEvents) > State#state.above_threshold,

            case HighTrending andalso Value > HighValue of
                true ->
                    case length(State#state.new_nums) < ?TREND_SIZE of
                        true ->
                            State1 = State#state{
                                n_events=NEvents,
                                high_trending=HighTrending,
                                new_nums=incr(Value, State#state.new_nums)
                            };
                        false ->
                            State1 = State#state{
                                n_events=NEvents,
                                high_trending=HighTrending
                            }
                    end,
                    case length(State1#state.new_nums) of
                        ?TREND_SIZE -> new_high_values(State1);
                        _ -> State1
                    end;
                false ->
                    State#state{
                        n_events=NEvents,
                        nums=incr_below(Value, State#state.nums),
                        high_trending=HighTrending,
                        n_above_max=case Value =< HighValue of 
                                        true -> State#state.n_above_max;
                                        false -> State#state.n_above_max+1
                                    end
                    }
            end;
        _ ->
            % Sample the first SAMPLE_SIZE different values
            case lists:keytake(Value, 1, State#state.nums) of
                {value, {Value,Count}, RestNums} ->
                    State1 = State#state{
                        n_events=NEvents,
                        nums=[{Value,Count+1}|RestNums]
                    };
                false ->
                    State1 = State#state{
                        n_events=NEvents,
                        nums=[{Value,1}|State#state.nums]
                    }
            end,
            case length(State1#state.nums) of
                ?SAMPLE_SIZE -> init_below(State1);
                _ -> State1
            end
    end.


-spec percentile(#state{}) -> non_neg_integer().
percentile(State) ->
    case length(State#state.nums) < ?SAMPLE_SIZE of
        true -> percentile_1(init_below(State));
        false -> percentile_1(State)
    end.

    percentile_1(State) ->
        Target = erlang:trunc(State#state.n_events * State#state.percentile),
        find_percentile(Target, State#state.nums).
    
    % When we don't find the target then our algorithm above was wrong.
    find_percentile(Target, [{N,B}|_]) when B >= Target ->
        N;
    find_percentile(Target, [_|Ns]) ->
        find_percentile(Target, Ns).


new_high_values(State) ->
    {_,LastBelow} = lists:last(State#state.nums),
    [{NewHighVal,NewHighBelow}|NewNumsBelow] = zip_below(LastBelow, lists:sort(State#state.new_nums), []),
    NewNumsBelow1 = lists:reverse([{NewHighVal, NewHighBelow+State#state.n_above_max}| NewNumsBelow]),
    {NumsBeforeBelow,_} = lists:split(length(NewNumsBelow1), State#state.nums), 

    State#state{
        nums=NumsBeforeBelow++NewNumsBelow1,
        high_trending=false,
        n_above_max=0,
        new_nums=[]
    }. 


incr_below(V, Ns) ->
    incr_below(V, Ns, []).
    
    incr_below(V, [{N,BelowN}|Ns], Acc) when V =< N ->
        incr_below(V, Ns, [{N,BelowN+1}|Acc]);
    incr_below(V, [N|Ns], Acc) ->
        incr_below(V, Ns, [N|Acc]);
    incr_below(_, [], Acc) ->
        lists:reverse(Acc).


% Replace the sample count with the number of samples below the value.
init_below(State) ->
    Nums = lists:sort(State#state.nums),
    State#state{
        nums=lists:reverse(zip_below(0, Nums, []))
    }.

zip_below(_Total, [], Acc) ->
    Acc;
zip_below(Total, [{N,Count}|Ns], Acc) ->
    Total1 = Count+Total,
    zip_below(Total1, Ns, [{N,Total1}|Acc]).


incr(N,Ns) ->
    case lists:keytake(N, 1, Ns) of
        {value, {N,Value}, Ns1} -> [{N,Value+1}|Ns1];
        false -> [{N,1}|Ns]
    end.
