## Data Types

### MLBPlayer

-   playerId :: MLBPlayerId
-   firstName :: String
-   lastName :: String
-   team :: TeamId
-   positions :: [String]
-   isOnInjuredList :: Bool
-   countStats :: Bool

### MLBTeam

-   teamId :: TeamId
-   teamName :: String
-   teamAbbreviation :: String
-   players :: [MLBPlayerId]
-   homePark :: BallparkId
-   league :: League
-   division :: Division

### League

-   AmericanLeague
-   NationalLeague

### Division

-   East
-   Central
-   West

### Ballpark

-   ballparkId :: String
-   ballparkName :: String

### Position

-   Pitcher
-   Catcher
-   FirstBase
-   SecondBase
-   ThirdBase
-   Shortstop
-   Outfield

### StatType

-   BattingStat BattingStats
-   PitchingStat PitchingStats

### Stats

-   player :: MLBPlayerId
-   statType :: StatType

### BattingStats

-   singles :: Int
-   doubles :: Int
-   triples :: Int
-   homeRuns :: Int
-   rbi :: Int
-   runs :: Int
-   baseOnBalls :: Int
-   stolenBases :: Int
-   strikeouts :: Int
-   caughtStealing :: Int
-   hitByPitch :: Int
-   fpts :: Double
-   fppg :: Double
-   gp :: Int
-   tpa :: Int
-   ab :: Int
-   hits :: Int
-   avg :: Double
-   obp :: Double
-   slg :: Double
-   ops :: Double
-   tb :: Int
-   sb :: Int
-   cs :: Int
-   hbp :: Int
-   sh :: Int
-   sf :: Int
-   e :: Int
-   a :: Int
-   gdp :: Int

### PitchingStats

-   wins :: Int
-   losses :: Int
-   era :: Double
-   saves :: Int
-   qualityStarts :: Int
-   strikeoutsPitcher :: Int
-   baseOnBallsPitcher :: Int
-   inningsPitched :: Double
-   hitsAllowed :: Int
-   earnedRuns :: Int
-   hitBatsman :: Int
-   fptsPitcher :: Double
-   fppgPitcher :: Double
-   gpPitcher :: Int
-   gs :: Int
-   cg :: Int
-   sho :: Int
-   hld :: Int
-   gbPercent :: Maybe Double
-   so9 :: Double
-   bb9 :: Double
## Functions

### battingStatValues

-   Input: Stats
-   Output: [Double]
-   Description: Extracts values of batting stats from the given 'Stats' object.


### pitchingStatValues

-   Input: Stats
-   Output: [Double]
-   Description: Extracts values of pitching stats from the given 'Stats' object.