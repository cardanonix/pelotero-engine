## Data Types

### GameId

-   newtype GameId = GameId String

### HalfInning

-   Top
-   Bottom

### PlayEventType

-   Single
-   Double
-   Triple
-   HomeRun
-   Walk
-   Strikeout
-   HitByPitch
-   FieldersChoice
-   GroundOut
-   FlyOut
-   LineOut
-   GroundIntoDoublePlay
-   GroundIntoTriplePlay
-   StolenBase
-   CaughtStealing
-   WildPitch
-   PassedBall
-   Balk
-   Error
-   SacrificeBunt
-   SacrificeFly
-   IntentionalWalk
-   DefensiveIndifference
-   Interference
-   FieldObstruction
-   OtherPlay

### PlayEvent

-   playEventType :: PlayEventType
-   inning :: Int
-   halfInning :: HalfInning
-   batter :: MLBPlayerId
-   pitcher :: MLBPlayerId
-   gameID :: GameID
-   teamIDs :: (TeamID, TeamID)
-   runnersAdvanced :: [(MLBPlayerId, Int)]
-   runsScored :: Int
-   rbi :: Int
-   outs :: (Int, Int)
-   errors :: Int

## Functions

### Generate Random Data

#### generateRandomGameId

-   Output: IO GameId

#### generateRandomPlayEventType

-   Output: IO PlayEventType

#### allPlayEventTypes

-   Output: [PlayEventType]

#### generateRandomInning

-   Output: IO Int

#### generateRandomHalfInning

-   Output: IO HalfInning

#### generateRandomBatter

-   Input: [MLBPlayer]
-   Output: IO MLBPlayerId

#### generateRandomPlay

-   Input: [MLBPlayer]
-   Output: IO PlayEvent

### Record Play

#### recordPlay

-   Input: Scorecard, Play
-   Output: Scorecard

### Simulate Inning

#### simulateInning

-   Input: Int, HalfInning
-   Output: IO Scorecard