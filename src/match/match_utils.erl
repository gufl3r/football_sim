-module(match_utils).
-include("match.hrl").

-export([team/2]).

team(Match, HasBall) ->
	if HasBall ->
		case Match#match.ball_possession of
			1 -> Match#match.team1;
			2 -> Match#match.team2
		end;
	true ->
		case Match#match.ball_possession of
			1 -> Match#match.team2;
			2 -> Match#match.team1
		end
	end.