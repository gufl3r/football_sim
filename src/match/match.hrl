-record(match,
{
	match_id,
	team1,
	team2,
	interval = 2.5,
	delay = 1000, % delay in milliseconds
	day = undefined,
	goals_team1 = 0,
	goals_team2 = 0,
	status = scheduled, % Possible values: scheduled, in_progress, finished
	time = 0,
	ball_position = 1, % 0 for team1, 1 for midfield, 2 for team2
	ball_possession = rand:uniform(2) % 1 for team1, 2 for team2
}).

-record(match_summary,
{
	ball_possession_team1,
	ball_possession_team2,
	shots_team1,
	shots_team2,
	passes_team1,
	passes_team2,
	successful_tackles_team1,
	successful_tackles_team2
}).