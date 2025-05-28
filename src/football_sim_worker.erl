-module(football_sim_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_simulation/0, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

start_simulation() ->
    gen_server:cast(?MODULE, start_simulation).

handle_cast(start_simulation, State) ->
    Team1 = team_main:new("Corinthians", "COR", {80, 85, 75}),
    Team2 = team_main:new("Palmeiras", "PAL", {78, 82, 80}),
    Match = match_main:new(0, Team1, Team2, #{day => 1}),
    match_main:print(Match, present),
    timer:sleep(1000),
    match_main:advance_status(Match),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.