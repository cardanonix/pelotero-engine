
for the initial lineup: ``http://statsapi.mlb.com/api/v1/game/718780/boxscore/JSON.teams.home.battingOrder.``
``http://statsapi.mlb.com/api/v1/game/718780/boxscore/JSON.teams.away.battingOrder.``

JSON.teams.home.players.MLBPlayerId


``http://statsapi.mlb.com/api/v1/game/718780/boxscore/JSON.teams.home.players.ID680776.stats.batting.``

## Batter Box Scores

``JSON.teams.home/away.players.ID######.stats.batting.``

``atBats :: Int
``atBatsPerHomeRun :: "-.--"  String w/ Double
``baseOnBalls :: Int
``catchersInterference :: Int
``caughtStealing :: Int
``doubles :: Int
``flyOuts :: Int
``gamesPlayed :: Int
``groundIntoDoublePlay : Int
``groundIntoTriplePlay :: Int
``groundOuts :: Int
``hitByPitch :: Int
``hits :: Int
``homeRuns :: Int
``intentionalWalks :: Int
``leftOnBase :: Int
``pickoffs :: Int
``plateAppearances :: Int
``rbi :: Int
``runs :: Int
``sacBunts :: Int
``sacFlies :: Int
``stolenBasePercentage :: Int
``stolenBases :: Int
``strikeOuts :: Int
``summary :: String (for example "1-3")
``totalBases :: Int
``triples :: Int

## Pitcher Box Scores
``http://statsapi.mlb.com/api/v1/game/694363/boxscore/JSON.teams.away.players.ID680776.stats.pitching.``

``JSON.teams.away.players.ID######.stats.pitching.``

``airOuts :: Int
``atBats  :: Int
``balks :: Int
``balls :: Int
``baseOnBalls :: Int
``battersFaced :: Int
``blownSaves :: Int
``catchersInterference :: Int
``caughtStealing :: Int
``completeGames :: Int
``doubles :: Int
``earnedRuns :: Int
``flyOuts :: Int
``gamesFinished :: Int
``gamesPitched :: Int
``gamesPlayed :: Int
``gamesStarted :: Int
``groundOuts :: Int
``hitBatsmen :: Int
``hitByPitch :: Int
``hits :: Int
``holds :: Int
``homeRuns :: Int
``homeRunsPer9  :: "0.00" String as Double
``inheritedRunners :: Int
``inheritedRunnersScored :: Int
``inningsPitched :: String as Double
``intentionalWalks :: Int
``losses :: Int
``note :: String (for example: "(H, 1)")
``numberOfPitches :: Int
``outs :: Int
``passedBall :: Int
``pickoffs :: Int
``pitchesThrown :: Int
``rbi :: Int
``runs :: Int
``runsScoredPer9 :: String (representing a Double "4.50")
``sacBunts :: Int
``sacFlies :: Int
``saveOpportunities :: Int
``saves :: Int
``shutouts :: Int
``stolenBasePercentage :: String (as Double ".---")
``stolenBases :: Int
``strikeOuts :: Int
``strikePercentage :: String (for example: ".430")
``strikes :: Int
``summary :: String (for example "2.0 IP, ER, K, 3 BB")
``triples :: Int
``wildPitches :: Int
``wins :: Int
