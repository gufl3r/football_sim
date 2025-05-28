-module(match_main).
-include("match/match.hrl").
-include("team/team.hrl").

-export([new/4, print/2, advance_status/1, match_loop/1]).

new(MatchId, Team1, Team2, Config) ->
    BaseMatch = #match{
        matchId = MatchId,
        team1 = Team1,
        team2 = Team2
    },
    Match1 = case maps:is_key(interval, Config) of
        true -> BaseMatch#match{interval = maps:get(interval, Config)};
        false -> BaseMatch
    end,
    Match2 = case maps:is_key(day, Config) of
        true -> Match1#match{day = maps:get(day, Config)};
        false -> Match1
    end,
    Match3 = case maps:is_key(delay, Config) of
        true -> Match2#match{delay = maps:get(delay, Config)};
        false -> Match2
    end,
    Match3.
    

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
                    io:format("Match ~p: ~s vs ~s, score: ~p - ~p, day: ~p, time: ~p~n",
                    [Match#match.matchId,
                     Match#match.team1#team.name, Match#match.team2#team.name,
                     Match#match.goalsTeam1, Match#match.goalsTeam2,
                     Match#match.day, Match#match.time]);
                _ ->
                    io:format("Match should be in in_progress status to show current summary.~n")
            end;
        % match summary shows the final score and the teams that played
        summary ->
            case Match#match.status of
                finished ->
                    io:format("Match ~p: ~s vs ~s, final score: ~p - ~p, day: ~p, finished~n",
                    [Match#match.matchId,
                     Match#match.team1#team.name, Match#match.team2#team.name,
                     Match#match.goalsTeam1, Match#match.goalsTeam2,
                     Match#match.day]);
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
            print(Match1, current),
            Match2 = advance_status(Match1),
            print(Match2, summary);
        true ->
            print(Match1, current),
            match_loop(Match1)
    end.