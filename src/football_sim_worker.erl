-module(football_sim_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("-Simulador de futebol-~n"),
    Team1 = team_main:new("Time Azul", "TAZ", {80, 75, 78}),
    Team2 = team_main:new("Time Vermelho", "TVM", {77, 82, 74}),
    team_main:print(Team1, scoreboard),
    team_main:print(Team2, scoreboard),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.