%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Maintain a lookup table with statz servers.

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

-module(statz_register).
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

-export([
    new/1,
    update/2,
    incr/1,
    summary/1,
    locate/1,
    delete/1,
    
    match/1
]).

-define(ETSTABLE, statz_register_ets).

%%====================================================================
%% API
%%====================================================================


%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() -> 
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


-spec new(term()) -> {ok, pid()}.
new(Name) -> 
    case locate(Name) of
        {ok, Pid} -> {ok, Pid};
        not_found -> gen_server:call(?MODULE, {new, Name})
    end.


-spec update(non_neg_integer(), term()) -> ok | not_found.
update(Value, Name) ->
    case locate(Name) of
        {ok, Pid} -> statz_server:update(Value, Pid);
        not_found -> not_found
    end.


-spec incr(term()) -> ok | not_found.
incr(Name) ->
    case locate(Name) of
        {ok, Pid} -> statz_server:incr(Pid);
        not_found -> not_found
    end.


-spec summary(term()) -> {ok, [{atom(), integer()}]} | not_found.
summary(Name) ->
    case locate(Name) of
        {ok, Pid} -> statz_server:summary(Pid);
        not_found -> not_found
    end.


-spec locate(term()) -> {ok, pid()} | not_found.
locate(Name) ->
    case ets:lookup(?ETSTABLE, Name) of
        [{Name, Pid}] -> {ok, Pid};
        [] -> not_found
    end.
    
-spec delete(term()) -> ok | not_found.
delete(Name) ->
    case locate(Name) of
        {ok, _Pid} -> gen_server:call(?MODULE, {delete, Name});
        not_found -> not_found
    end.


-spec match(Pattern :: term()) -> [ term() ].
match(Pattern) ->
    [ Key || {Key,_Pid} <- ets:match_object(?ETSTABLE, {Pattern, '_'}) ].

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server, allocate a new ets table for registering all statz servers
-spec init(list()) -> {ok, undefined}.
init(_Args) ->
    ets:new(?ETSTABLE, [set, named_table, protected, {heir, none}, {read_concurrency, true}]),
    {ok, undefined}.

handle_call({new, Name}, _From, State) ->
    case ets:lookup(?ETSTABLE, Name) of
        [{Name, Pid}] ->
            {reply, {ok, Pid}, State};
        [] ->
            {ok, Pid} = statz_server:start_link(),
            ets:insert(?ETSTABLE, [{Name,Pid}]),
            {reply, {ok, Pid}, State}
    end;
handle_call({delete, Name}, _From, State) ->
    case ets:lookup(?ETSTABLE, Name) of
        [{Name, Pid}] ->
            statz_server:stop(Pid),
            ets:delete(?ETSTABLE, Name),
            {reply, ok, State};
        [] ->
            {reply, not_found, State}
    end.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    