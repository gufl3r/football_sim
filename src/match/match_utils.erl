-module(match_utils).
-include("team/team.hrl").
-include("match/match.hrl").

-export([team_name/2]).

team_name(Match, HasBall) ->
	if HasBall ->
		case Match#match.ballPossession of
			1 -> Match#match.team1#team.name;
			2 -> Match#match.team2#team.name
		end;
	true ->
		case Match#match.ballPossession of
			1 -> Match#match.team2#team.name;
			2 -> Match#match.team1#team.name
		end
	end.