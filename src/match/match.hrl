-record
(
    match,
    {
        matchId,
        team1,
        team2,
        interval = 5,
        delay = 1000, % delay in milliseconds
        day = undefined,
        goalsTeam1 = 0,
        goalsTeam2 = 0,
        status = scheduled, % Possible values: scheduled, in_progress, finished
        time = 0,
        ballPosition = 1, % 0 for team1, 1 for midfield, 2 for team2
        ballPossession = 1 % 1 for team1, 2 for team2
    }
).