### Data Types

#### FantasyLeagueId

-   newtype FantasyLeagueId

#### FantasyLeague

-   leagueId :: FantasyLeagueId
-   managers :: Map.Map ManagerId Manager
-   rosterSize :: Int
-   activeLineupSize :: Int
-   reserveSize :: Int
-   injuredListSize :: Int
-   currentDate :: UTCTime

#### ManagerId

-   newtype ManagerId

#### Record

-   type Record = (Int, Int)

#### Manager

-   managerId :: ManagerId
-   fantasyteamName :: String
-   fantasyleague :: FantasyLeagueId
-   draftPicks :: [MLBPlayerId]
-   activeLineup :: [MLBPlayerId]
-   reservePlayers :: [MLBPlayerId]
-   injuredList :: [MLBPlayerId]
-   hasDualThreat :: Bool
-   record :: Record

#### Points

-   battingPoints :: BattingPoints
-   pitchingPoints :: PitchingPoints

#### BattingPoints

-   fb_single :: Double
-   fb_double :: Double
-   fb_triple :: Double
-   fb_homeRun :: Double
-   fb_rbi :: Double
-   fb_run :: Double
-   fb_baseOnBall :: Double
-   fb_stolenBase :: Double
-   fb_strikeout :: Double
-   fb_caughtStealing :: Double
-   fb_hitByPitch :: Double

#### PitchingPoints

-   fp_win :: Double
-   fp_save :: Double
-   fp_qualityStart :: Double
-   fp_strikeoutPitcher :: Double
-   fp_baseOnBallPitcher :: Double
-   fp_inningPitched :: Double
-   fp_hitAllowed :: Double
-   fp_earnedRun :: Double
-   fp_hitBatsman :: Double
-   fp_loss :: Double

### Functions

#### battingPointValues

-   Input: Points, BattingStats
-   Output: [Double]

#### pitchingPointValues

-   Input: Points, PitchingStats
-   Output: [Double]

#### calculatePoints

-   Input: Points, MLBPlayer, Stats
-   Output: Points

#### sumPitchingPoints

-   Input: Points, PitchingStats
-   Output: Double

#### sumBattingPoints

-   Input: Points, BattingStats
-   Output: Double

#### defaultPoints

-   Output: Points