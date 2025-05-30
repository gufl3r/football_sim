-module(team_main).
-include("team.hrl").

-export([new/3, win/1, draw/1, lose/1, print/2]).

new(Name, Acronym, Stats) ->
    #team{
        name = Name,
        acronym = Acronym,
        defense_strength = element(1, Stats),
        midfield_strength = element(2, Stats),
        attack_strength = element(3, Stats)
    }.

win(Team) ->
    Team#team{
        matches_won = Team#team.matches_won + 1,
        points = Team#team.points + 3
    }.

draw(Team) ->
    Team#team{
        matches_drawn = Team#team.matches_drawn + 1,
        points = Team#team.points + 1
    }.

lose(Team) ->
    Team#team{matches_lost = Team#team.matches_lost + 1}.

print(Team, Preset) ->
    case Preset of
        full ->
            io:format(
                "Team: ~s (~s)~n"
                "Defense: ~p, Attack: ~p, Midfield: ~p~n"
                "Points: ~p, Goals Scored: ~p, Goals Conceded: ~p~n"
                "Wins: ~p, Draws: ~p, Losses: ~p~n",
                [Team#team.name, Team#team.acronym,
                 Team#team.defense_strength, Team#team.attack_strength, Team#team.midfield_strength,
                 Team#team.points, Team#team.goals_scored, Team#team.goals_conceded,
                 Team#team.matches_won, Team#team.matches_drawn, Team#team.matches_lost]
            );
        identifiers ->
            io:format("~s (~s)~n", [Team#team.name, Team#team.acronym]);
        scoreboard ->
            io:format(
                "[~-20s ~s ~p ~p ~p ~p ~p ~p ~p ~p ~p]~n",
                [Team#team.name, Team#team.acronym,
                 Team#team.points, Team#team.goals_scored, Team#team.goals_conceded,
                 Team#team.matches_won, Team#team.matches_drawn, Team#team.matches_lost,
                 Team#team.defense_strength, Team#team.attack_strength, Team#team.midfield_strength]
            );
        _ ->
            io:format("Team: ~p~n", [Team])
    end.