-module(match_step).
-include("team/team.hrl").
-include("match/match.hrl").
-define(ATTACK_MARKING_MULTIPLIER, 0.85).
-define(MIDFIELD_MARKING_MULTIPLIER, 0.9).

-export([step/1, advance_to_midfield/1, advance_to_attack/1, advance_to_goal/1, shoot_to_goal/1, next_action/3, after_miss_shoot/1]).
step(Match) ->
	% time com a bola no meio campo compara sua força para passar do meio campo
	% se for mais fraco, perde a posse de bola, bola continua no meio campo
	% se for mais forte, ganha a bola, avança para o ataque
	% time com a bola no ataque compara sua força de ataque com a defesa do adversário
	% se for mais fraco, perde a posse de bola
	% se for mais forte, avança para o gol do adversário
	% ataque tem chance calculada de fazer gol, sem depender do adversário
	% chance calculada pode ser algo como: primeiro: roda 50/50: chute de longe ou de perto
	% se for de perto, chance de gol é 70%, se for de longe, chance de gol é 30%, essas porcentagens + algum modificador de força do ataque
	% tendo a bola na defesa ganhando ou empatando, tem 15% de chance de tentar ligação direta com o ataque
	% tendo a bola na defesa perdendo, tem 40% de chance de tentar ligação direta com o ataque, essa porcentagem + algum modificador de "tamanho de derrota", por exemplo, se estiver perdendo de 1 gol, a chance de ligação direta é 40%, se estiver perdendo de 2 gols, a chance de ligação direta é 60%, se estiver perdendo de 3 gols ou mais, a chance de ligação direta na verdade cai, pois o time deve estar desmotivado
	% chance de sucesso em ligação direta (a bola chegar no ataque) é 30% + algum modificador de força da defesa do time que está com a bola
	% se um time estiver ganhando de um time melhor que ele, perde ataque aos poucos, a força na verdade é distribuida entre meio campo e defesa, representando o time se fechando na defesa
	% se um time estiver perdendo de um time pior que ele, perde defesa aos poucos, a força na verdade é distribuida entre meio campo e ataque, representando o time se abrindo no ataque
	% para contra ataque, acho que depois de conseguir impedir um ataque, com 20% de chance + força da defesa, o time pode receber um buff em meio campo e ataque, esse buff dura por apenas 2 passos (defesa para meio campo, meio campo para ataque), e depois disso o time volta a sua força normal
	% em qualquer estado do jogo, há 30% de chance de nenhuma ação acontecer, ou seja, apenas o tempo muda (https://football-observatory.com/Effective-playing-time-in-37-European-2150) (a chance de nenhuma ação acontecer foi dimuida de 42% para 30% para aumentar a dinâmica do jogo, já que o jogo é simulado em poucos passos)
	IsEffectivePlayTime = rand:uniform(100) > 30,
	case IsEffectivePlayTime of
		false ->
			io:format("~s was not creative enough to continue the play~n", [match_utils:team_name(Match, true)]),
			Match;
		true ->
			next_action(Match#match.ballPosition, Match#match.ballPossession, Match)
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
	io:format("~s tries to advance to midfield~n", [match_utils:team_name(Match, true)]),
	DefenderDefenseStrength = case Match#match.ballPossession of
		1 -> Match#match.team2#team.defenseStrenght;
		2 -> Match#match.team1#team.defenseStrenght
	end,
	AttackerMidfieldStrength = case Match#match.ballPossession of
		1 -> Match#match.team1#team.midfieldStrenght;
		2 -> Match#match.team2#team.midfieldStrenght
	end,
	ClashResult = DefenderDefenseStrength - (AttackerMidfieldStrength * (1.1 - (rand:uniform() * 0.2)) * ?ATTACK_MARKING_MULTIPLIER),
	% ball goes from defense to midfield if defenders win the clash
	if
		ClashResult > 0 -> % defender wins the clash
			io:format("~s successfully advances to midfield!~n", [match_utils:team_name(Match, true)]),
			Match#match{ballPosition = 1}; % move ball to midfield
		true -> % attacker wins the clash
			io:format("~s' attackers steal the ball in defense!~n", [match_utils:team_name(Match, false)]),
			Match#match{ballPossession = 3 - Match#match.ballPossession}
	end.

advance_to_attack(Match) ->
	% time com a bola no meio campo compara sua força de meio campo com a defesa do adversário
	io:format("~s tries to advance to attack~n", [match_utils:team_name(Match, true)]),
	AttackerMidfieldStrength = case Match#match.ballPossession of
		1 -> Match#match.team1#team.midfieldStrenght;
		2 -> Match#match.team2#team.midfieldStrenght
	end,
	DefenderMidfieldStrength = case Match#match.ballPossession of
		1 -> Match#match.team2#team.midfieldStrenght;
		2 -> Match#match.team1#team.midfieldStrenght
	end,
	ClashResult = AttackerMidfieldStrength - (DefenderMidfieldStrength * (1.1 - (rand:uniform() * 0.2)) * ?MIDFIELD_MARKING_MULTIPLIER),
	if 
		ClashResult > 0 -> % attacker wins the clash
			io:format("~s successfully advances to attack!~n", [match_utils:team_name(Match, true)]),
			case Match#match.ballPossession of
				1 -> Match#match{ballPosition = 2}; % move ball to attack for team 1
				2 -> Match#match{ballPosition = 0} % move ball to attack for team 2
			end;
		true -> % defender wins the clash
			io:format("~s won the ball in midfield!~n", [match_utils:team_name(Match, false)]),
			Match#match{ballPossession = 3 - Match#match.ballPossession} % switch possession to the other team
	end.

advance_to_goal(Match) ->
	io:format("~s tries to advance to goal~n", [match_utils:team_name(Match, true)]),
	AttackerAttackStrength = case Match#match.ballPossession of
		1 -> Match#match.team1#team.attackStrenght;
		2 -> Match#match.team2#team.attackStrenght
	end,
	DefenderDefenseStrength = case Match#match.ballPossession of
		1 -> Match#match.team2#team.defenseStrenght;
		2 -> Match#match.team1#team.defenseStrenght
	end,
	ClashResult = (AttackerAttackStrength * (1.1 - (rand:uniform() * 0.2))) - DefenderDefenseStrength,
	if
		ClashResult > 0 -> % attacker wins the clash
			io:format("~s successfully advances to goal!~n", [match_utils:team_name(Match, true)]),
			shoot_to_goal(Match);
		true -> % defender wins the clash
			io:format("~s's defenders stay strong and win the ball!~n", [match_utils:team_name(Match, false)]),
			Match#match{ballPossession = 3 - Match#match.ballPossession} % switch possession to the other team
	end.
	
shoot_to_goal(Match) ->
	TeamName = match_utils:team_name(Match, true),
	io:format("~s shoots to goal~n", [TeamName]),
	Chance = rand:uniform(100),
	ShootPosition = rand:uniform(2),
	case ShootPosition of
		1 -> % close shot
			if
				Chance =< 70 -> % 70% chance to score
					io:format("Goal! ~s scores with a close shot!~n", [TeamName]),
					Match1 = Match#match{ballPosition = 1}, % reset ball position to midfield after a goal
					case Match1#match.ballPossession of
						1 -> Match1#match{goalsTeam1 = Match1#match.goalsTeam1 + 1, ballPossession = 2};
						2 -> Match1#match{goalsTeam2 = Match1#match.goalsTeam2 + 1, ballPossession = 1}
					end;
				true ->
					io:format("Missed! ~s fails to score with a close shot.~n", [TeamName]),
					after_miss_shoot(Match)
			end;
		2 -> % long shot
			if
				Chance =< 30 -> % 30% chance to score
					io:format("Goal! ~s scores with a long shot!~n", [TeamName]),
					Match1 = Match#match{ballPosition = 1}, % reset ball position to midfield after a goal
					case Match1#match.ballPossession of
						1 -> Match1#match{goalsTeam1 = Match1#match.goalsTeam1 + 1, ballPossession = 2};
						2 -> Match1#match{goalsTeam2 = Match1#match.goalsTeam2 + 1, ballPossession = 1}
					end;
				true ->
					io:format("Missed! ~s fails to score with a long shot.~n", [TeamName]),
					after_miss_shoot(Match)
			end
	end.

after_miss_shoot(Match) ->
	% after a missed shot, the ball has:
	% - 70% chance to go to the other team
	% - 25% chance to stay with the team that missed the shot
	% - 5% chance of a rebound, where the attacking team immediately gets another chance to shoot
	Chance = rand:uniform(100),
	if 
		Chance =< 70 -> % 70% chance to go to the other team
			io:format("The ball goes to ~s's defenders after the missed shot.~n", [match_utils:team_name(Match, false)]),
			case Match#match.ballPossession of
				1 -> Match#match{ballPossession = 2};
				2 -> Match#match{ballPossession = 1}
			end;
		Chance =< 95 -> % 25% chance to stay with the team that missed the shot
			io:format("The ball stays with ~s's attackers after the missed shot.~n", [match_utils:team_name(Match, true)]),
			Match;
		true -> % 5% chance of a rebound
			io:format("Rebound! ~s's attackers get another chance to shoot!~n", [match_utils:team_name(Match, true)]),
			shoot_to_goal(Match)
	end.