-module(match_step).
-include("team/team.hrl").
-include("match.hrl").
-define(ATTACK_MARKING_MULTIPLIER, 0.85).
-define(MIDFIELD_MARKING_MULTIPLIER, 0.9).

-export([step/1, advance_to_midfield/1, advance_to_attack/1, advance_to_goal/1, shoot_to_goal/1, next_action/3, after_miss_shoot/1]).

step(Match) ->
    IsEffectivePlayTime = rand:uniform(100) > 30,
    case IsEffectivePlayTime of
        false ->
            TeamWithBall = match_utils:team(Match, true),
            io:format("~s was not creative enough to continue the play~n", [TeamWithBall#team.name]),
            Match;
        true ->
            next_action(Match#match.ball_position, Match#match.ball_possession, Match)
    end.

next_action(0, 1, Match) -> % defesa team 1
    advance_to_midfield(Match);
next_action(2, 2, Match) -> % defesa team 2
    advance_to_midfield(Match);
next_action(1, _, Match) -> % meio campo
    advance_to_attack(Match);
next_action(2, 1, Match) -> % ataque team 1
    advance_to_goal(Match);
next_action(0, 2, Match) -> % ataque team 2
    advance_to_goal(Match);
next_action(_, _, Match) -> % fallback
    Match.

advance_to_midfield(Match) ->
    % time com a bola na defesa compara sua força de defesa com a força de ataque do adversário
    TeamWithBall = match_utils:team(Match, true),
    TeamWithoutBall = match_utils:team(Match, false),
    io:format("~s tries to advance to midfield~n", [TeamWithBall#team.name]),
    DefenderDefenseStrength = TeamWithoutBall#team.defense_strength,
    AttackerAttackStrength = TeamWithBall#team.attack_strength,
    BlunderChance = rand:uniform(100),
    DefenderBlunderPotential = (96 - DefenderDefenseStrength)/2,
    if
        BlunderChance < DefenderBlunderPotential ->
            io:format("What a blunder! A ~s's defender handed the ball to the opposition in his own territory.~n", [TeamWithBall#team.name]),
            Match#match{ball_possession = 3 - Match#match.ball_possession};
        true ->
            ClashResult = DefenderDefenseStrength - (AttackerAttackStrength * (1.1 - (rand:uniform() * 0.2)) * ?ATTACK_MARKING_MULTIPLIER),
            % ball goes from defense to midfield if defenders win the clash
            if
                ClashResult > 0 -> % defender wins the clash
                    io:format("~s successfully advances to midfield!~n", [TeamWithBall#team.name]),
                    Match#match{ball_position = 1}; % move ball to midfield
                true -> % attacker wins the clash
                    io:format("~s' attackers steal the ball in defense!~n", [TeamWithoutBall#team.name]),
                    Match#match{ball_possession = 3 - Match#match.ball_possession}
            end
        end.

advance_to_attack(Match) ->
    % time com a bola no meio campo compara sua força de meio campo com a defesa do adversário
    TeamWithBall = match_utils:team(Match, true),
    TeamWithoutBall = match_utils:team(Match, false),
    io:format("~s tries to advance to attack~n", [TeamWithBall#team.name]),
    AttackerMidfieldStrength = TeamWithBall#team.midfield_strength,
    DefenderMidfieldStrength = TeamWithoutBall#team.midfield_strength,
    ClashResult = AttackerMidfieldStrength - (DefenderMidfieldStrength * (1.1 - (rand:uniform() * 0.2)) * ?MIDFIELD_MARKING_MULTIPLIER),
    if 
        ClashResult > 0 -> % attacker wins the clash
            io:format("~s successfully advances to attack!~n", [TeamWithBall#team.name]),
            case Match#match.ball_possession of
                1 -> Match#match{ball_position = 2}; % move ball to attack for team 1
                2 -> Match#match{ball_position = 0} % move ball to attack for team 2
            end;
        true -> % defender wins the clash
            io:format("~s won the ball in midfield!~n", [TeamWithoutBall#team.name]),
            Match#match{ball_possession = 3 - Match#match.ball_possession} % switch possession to the other team
    end.

advance_to_goal(Match) ->
    TeamWithBall = match_utils:team(Match, true),
    TeamWithoutBall = match_utils:team(Match, false),
    io:format("~s tries to advance to goal~n", [TeamWithBall#team.name]),
    AttackerAttackStrength = TeamWithBall#team.attack_strength,
    DefenderDefenseStrength = TeamWithoutBall#team.defense_strength,
    ClashResult = (AttackerAttackStrength * (1.1 - (rand:uniform() * 0.2))) - DefenderDefenseStrength,
    if
        ClashResult > 0 -> % attacker wins the clash
            io:format("~s successfully advances to goal!~n", [TeamWithBall#team.name]),
            shoot_to_goal(Match);
        true -> % defender wins the clash
            io:format("~s's defenders stay strong and win the ball!~n", [TeamWithoutBall#team.name]),
            Match#match{ball_possession = 3 - Match#match.ball_possession} % switch possession to the other team
    end.

shoot_to_goal(Match) ->
    TeamWithBall = match_utils:team(Match, true),
    io:format("~s shoots to goal~n", [TeamWithBall#team.name]),
    Chance = rand:uniform(100),
    ShootPosition = rand:uniform(2),
    case ShootPosition of
        1 -> % close shot
            if
                Chance =< 70 -> % 70% chance to score
                    io:format("Goal! ~s scores with a close shot!~n", [TeamWithBall#team.name]),
                    Match1 = Match#match{ball_position = 1}, % reset ball position to midfield after a goal
                    case Match1#match.ball_possession of
                        1 -> Match1#match{goals_team1 = Match1#match.goals_team1 + 1, ball_possession = 2};
                        2 -> Match1#match{goals_team2 = Match1#match.goals_team2 + 1, ball_possession = 1}
                    end;
                true ->
                    io:format("Missed! ~s fails to score with a close shot.~n", [TeamWithBall#team.name]),
                    after_miss_shoot(Match)
            end;
        2 -> % long shot
            if
                Chance =< 30 -> % 30% chance to score
                    io:format("Goal! ~s scores with a long shot!~n", [TeamWithBall#team.name]),
                    Match1 = Match#match{ball_position = 1}, % reset ball position to midfield after a goal
                    case Match1#match.ball_possession of
                        1 -> Match1#match{goals_team1 = Match1#match.goals_team1 + 1, ball_possession = 2};
                        2 -> Match1#match{goals_team2 = Match1#match.goals_team2 + 1, ball_possession = 1}
                    end;
                true ->
                    io:format("Missed! ~s fails to score with a long shot.~n", [TeamWithBall#team.name]),
                    after_miss_shoot(Match)
            end
    end.

after_miss_shoot(Match) ->
    % after a missed shot, the ball has:
    % - 70% chance to go to the other team
    % - 25% chance to stay with the team that missed the shot
    % - 5% chance of a rebound, where the attacking team immediately gets another chance to shoot
    TeamWithBall = match_utils:team(Match, true),
    TeamWithoutBall = match_utils:team(Match, false),
    Chance = rand:uniform(100),
    if 
        Chance =< 70 -> % 70% chance to go to the other team
            io:format("The ball goes to ~s's defenders after the missed shot.~n", [TeamWithoutBall#team.name]),
            case Match#match.ball_possession of
                1 -> Match#match{ball_possession = 2};
                2 -> Match#match{ball_possession = 1}
            end;
        Chance =< 95 -> % 25% chance to stay with the team that missed the shot
            io:format("The ball stays with ~s's attackers after the missed shot.~n", [TeamWithBall#team.name]),
            Match;
        true -> % 5% chance of a rebound
            io:format("Rebound! ~s's attackers get another chance to shoot!~n", [TeamWithBall#team.name]),
            shoot_to_goal(Match)
    end.