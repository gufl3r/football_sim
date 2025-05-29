-module(match_main).
-include("match/match.hrl").
-include("team/team.hrl").

-export([new/4, print/2, advance_status/1, match_loop/1]).

new(MatchId, Team1, Team2, Config) ->
	Match = #match
	{
		matchId = MatchId,
		team1 = Team1,
		team2 = Team2
	},
	lists:foldl(
        fun(Key, AccMatch) ->
            case Key of
                interval -> AccMatch#match{interval = maps:get(interval, Config)};
                day -> AccMatch#match{day = maps:get(day, Config)};
                delay -> AccMatch#match{delay = maps:get(delay, Config)};
                _ -> AccMatch
            end
        end,
		Match,
        maps:keys(Config)
    ).
	
print(Match, Preset) ->
	case Preset of
		% you can only present a match that has scheduled status
		% match presentation shows quick summary of the match in jornalistic style
		present ->
			case Match#match.status of
				scheduled ->
					io:format("The match between ~s and ~s is scheduled for day ~p.~n",
					[Match#match.team1#team.name, Match#match.team2#team.name, Match#match.day]);
				_ ->
					io:format("Match should be in scheduled status to be presented.~n")
			end;
		%current match summary in one line of a match in in_progress
		current ->
			case Match#match.status of
				in_progress ->
					io:format("Day: ~p, match ~p: ~s ~p vs ~p ~s ~p:00~n",
					[Match#match.day, Match#match.matchId,
					 Match#match.team1#team.acronym, Match#match.goalsTeam1,
					 Match#match.goalsTeam2, Match#match.team2#team.acronym,
					 round(Match#match.time)]);
				_ ->
					io:format("Match should be in in_progress status to show current summary.~n")
			end;
		% match summary shows the final score and the teams that played
		summary ->
			case Match#match.status of
				finished ->
					io:format("Day: ~p, match ~p: ~s ~p vs ~p ~s finished!~n",
					[Match#match.day, Match#match.matchId,
					 Match#match.team1#team.acronym, Match#match.goalsTeam1,
					 Match#match.goalsTeam2, Match#match.team2#team.acronym]);
				_ ->
					io:format("Match should be in finished status to show summary.~n")
			end;
		_ ->
			io:format("Match: ~p~n", [Match])
	end.

advance_status(Match) ->
	case Match#match.status of
		scheduled ->
			match_loop(Match#match{status = in_progress});
		in_progress ->
			Match#match{status = finished};
		finished ->
			io:format("Match ~p is already finished.~n", [Match#match.matchId]),
			Match;
		_ ->
			io:format("Fatal error: match ~p has invalid status~n", [Match#match.matchId]),
			Match
	end.

% match loop without receive for now, just loops through match times
match_loop(Match) ->
	timer:sleep(Match#match.delay),
	Match1 = match_step:step(Match#match{time = Match#match.time + Match#match.interval}),
	if 
		Match1#match.time >= 90 ->
			Match2 = Match1#match{time = 90},
			print(Match2, current),
			print(advance_status(Match2), summary);
		true ->
			print(Match1, current),
			match_loop(Match1)
	end.