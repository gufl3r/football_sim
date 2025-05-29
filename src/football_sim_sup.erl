-module(football_sim_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Children = [
		{football_sim_worker, {football_sim_worker, start_link, []},
		 permanent, 5000, worker, [football_sim_worker]}
	],
	{ok, {{one_for_one, 1, 5}, Children}}.