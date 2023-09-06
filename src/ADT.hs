{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M

-- Top level data type
data GameData = GameData
    { teams :: Teams
    } deriving (Show, Eq)

-- Teams data type
data Teams = Teams
    { away :: TeamData
    , home :: TeamData
    } deriving (Show, Eq)

data TeamData = TeamData
    { players :: M.Map Text Player
    } deriving (Show, Eq)

instance FromJSON TeamData where
    parseJSON = withObject "TeamData" $ \v ->
        TeamData <$> v .: "players"

type Players = [(Text, Player)]

-- Player data structure
data Player = Player
    { person          :: Person
    , parentTeamId    :: Int
    , allPositions    :: [Position]
    , status          :: Status
    , stats           :: PlayerStats
    } deriving (Show, Eq)

data Person = Person
    { personId   :: Int
    , fullName :: Text
    } deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v ->
        Person <$> v .: "id"
               <*> v .: "fullName"

data Position = Position
    { code :: Text
    } deriving (Show, Eq)

data Status = Status
    { code :: Text
    } deriving (Show, Eq)


data PlayerStats = PlayerStats
    { batting  :: Maybe BattingStats
    , pitching :: Maybe PitchingStats
    } deriving (Show, Eq)

data BattingStats = BattingStats
    { gamesPlayed           :: Int
    , flyOuts               :: Int
    , groundOuts            :: Int
    , runs                  :: Int
    , doubles               :: Int
    , triples               :: Int
    , homeRuns              :: Int
    , strikeOuts            :: Int
    , baseOnBalls           :: Int
    , intentionalWalks      :: Int
    , hits                  :: Int
    , hitByPitch            :: Int
    , atBats                :: Int
    , caughtStealing        :: Int
    , stolenBases           :: Int
    , groundIntoDoublePlay  :: Int
    , groundIntoTriplePlay  :: Int
    , plateAppearances      :: Int
    , totalBases            :: Int
    , rbi                   :: Int
    , leftOnBase            :: Int
    , sacBunts              :: Int
    , sacFlies              :: Int
    , catchersInterference  :: Int
    , pickoffs              :: Int
    , note                  :: Text
    , summary               :: Text
    , stolenBasePercentage  :: Double
    , atBatsPerHomeRun      :: Double
    } deriving (Show, Eq)

data PitchingStats = PitchingStats
    { summary               :: Text
    , gamesPlayed           :: Int
    , gamesStarted          :: Int
    , flyOuts               :: Int
    , groundOuts            :: Int
    , airOuts               :: Int
    , runs                  :: Int
    , doubles               :: Int
    , triples               :: Int
    , homeRuns              :: Int
    , strikeOuts            :: Int
    , baseOnBalls           :: Int
    , intentionalWalks      :: Int
    , hits                  :: Int
    , hitByPitch            :: Int
    , atBats                :: Int
    , caughtStealing        :: Int
    , stolenBases           :: Int
    , stolenBasePercentage  :: Text
    , numberOfPitches       :: Int
    , inningsPitched        :: Text
    , wins                  :: Int
    , losses                :: Int
    , saves                 :: Int
    , saveOpportunities     :: Int
    , holds                 :: Int
    , blownSaves            :: Int
    , earnedRuns            :: Int
    , battersFaced          :: Int
    , outs                  :: Int
    , gamesPitched          :: Int
    , completeGames         :: Int
    , shutouts              :: Int
    , pitchesThrown         :: Int
    , balls                 :: Int
    , strikes               :: Int
    , strikePercentage      :: Text
    , hitBatsmen            :: Int
    , balks                 :: Int
    , wildPitches           :: Int
    , pickoffs              :: Int
    , rbi                   :: Int
    , gamesFinished         :: Int
    , runsScoredPer9        :: Text
    , homeRunsPer9          :: Text
    , inheritedRunners      :: Int
    , inheritedRunnersScored:: Int
    , catchersInterference  :: Int
    , sacBunts              :: Int
    , sacFlies              :: Int
    , passedBall            :: Int
    } deriving (Show, Eq)

-- JSON instances
instance FromJSON GameData where
    parseJSON = withObject "GameData" $ \v ->
        GameData <$> v .: "teams"

