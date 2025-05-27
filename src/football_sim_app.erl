%%%-------------------------------------------------------------------
%% @doc football_sim public API
%% @end
%%%-------------------------------------------------------------------

-module(football_sim_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    football_sim_sup:start_link().

stop(_State) ->
    ok.

%% internal functions