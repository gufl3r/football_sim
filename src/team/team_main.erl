-module(team_main).
-include("team.hrl").

-export([new/3, add_goal/1, win/1, draw/1, lose/1, print/2]).

new(Name, Acronym, Stats) ->
    #team{
        name = Name,
        acronym = Acronym,
        defenseStrenght = element(1, Stats),
        attackStrenght = element(2, Stats),
        midfieldStrenght = element(3, Stats)
    }.

add_goal(Team) ->
    Team#team{goalsScored = Team#team.goalsScored + 1}.

win(Team) ->
    Team#team{
        matchesWon = Team#team.matchesWon + 1,
        points = Team#team.points + 3
    }.

draw(Team) ->
    Team#team{
        matchesDrawn = Team#team.matchesDrawn + 1,
        points = Team#team.points + 1
    }.

lose(Team) ->
    Team#team{matchesLost = Team#team.matchesLost + 1}.

print(Team, Preset) ->
    case Preset of
        full ->
            io:format(
                "Team: ~s (~s)~n"
                "Defense: ~p, Attack: ~p, Midfield: ~p~n"
                "Points: ~p, Goals Scored: ~p, Goals Conceded: ~p~n"
                "Wins: ~p, Draws: ~p, Losses: ~p~n",
                [Team#team.name, Team#team.acronym,
                 Team#team.defenseStrenght, Team#team.attackStrenght, Team#team.midfieldStrenght,
                 Team#team.points, Team#team.goalsScored, Team#team.goalsConceded,
                 Team#team.matchesWon, Team#team.matchesDrawn, Team#team.matchesLost]
            );
        identifiers ->
            io:format("~s (~s)~n", [Team#team.name, Team#team.acronym]);
        scoreboard ->
        io:format(
            "[~-20s ~s ~p ~p ~p ~p ~p ~p ~p ~p ~p]~n",
            [Team#team.name, Team#team.acronym,
            Team#team.points, Team#team.goalsScored, Team#team.goalsConceded,
            Team#team.matchesWon, Team#team.matchesDrawn, Team#team.matchesLost,
            Team#team.defenseStrenght, Team#team.attackStrenght, Team#team.midfieldStrenght]
        );
        _ ->
            io:format("Team: ~p~n", [Team])
    end.