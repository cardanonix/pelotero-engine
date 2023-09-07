{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as B (readFile)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM)


-- Top level data type
data GameData = GameData
    { teams :: Teams
    } deriving (Show, Eq)

-- JSON instances
instance FromJSON GameData where
    parseJSON = withObject "GameData" $ \v ->
        GameData <$> v .: "teams"

-- Teams data type
data Teams = Teams
    { away :: TeamData
    , home :: TeamData
    } deriving (Show, Eq)

instance FromJSON Teams where
    parseJSON = withObject "Teams" $ \v -> Teams
        <$> v .: "away"
        <*> v .: "home"

data TeamData = TeamData
    { players :: M.Map Text Player
    } deriving (Show, Eq)

hasValidPositions :: Value -> Bool
hasValidPositions val = case fromJSON val :: Result Player of
    Success player -> case allPositions player of
                        Just positions -> not (null positions)
                        Nothing -> False
    _ -> False

instance FromJSON TeamData where
    parseJSON = withObject "TeamData" $ \v -> do
        playersMap <- v .: "players" :: Parser (M.Map Text Value)
        let maybePlayersList = map (\(k, v) -> 
                if hasValidPositions v 
                then case fromJSON v of
                        Success player -> Just (k, player)
                        _ -> Nothing
                else Nothing) (M.toList playersMap)

        let validPlayers = M.fromList $ catMaybes maybePlayersList
        pure TeamData { players = validPlayers }

type Players = [(Text, Player)]

-- Player data structure
data Player = Player
    { person          :: Person
    , parentTeamId    :: Int
    , allPositions    :: Maybe [Position]
    , status          :: Status
    , stats           :: PlayerStats
    } deriving (Show, Eq)

instance FromJSON Player where
    parseJSON = withObject "Player" $ \v -> do
        person <- v .: "person"
        teamId <- v .: "parentTeamId"
        positions <- v .:? "allPositions"
        let validPositions = case positions of
                Just ps -> if null ps then Nothing else Just ps
                Nothing -> Nothing
        status <- v .: "status"
        stats <- v .: "stats"
        return $ Player person teamId validPositions status stats



data Person = Person
    { personId   :: Int
    , fullName :: Text
    } deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v ->
        Person <$> v .: "id"
               <*> v .: "fullName"

data Position = Position
    { pos_code :: Text
    } deriving (Show, Eq)

instance FromJSON Position where
    parseJSON = withObject "Position" $ \v -> Position
        <$> v .: "code"

data Status = Status
    { status_code :: Text
    } deriving (Show, Eq)

instance FromJSON Status where
    parseJSON = withObject "Status" $ \v -> Status
        <$> v .: "code"

data PlayerStats = PlayerStats
    { batting  :: Maybe BattingStats
    , pitching :: Maybe PitchingStats
    } deriving (Show, Eq)

instance FromJSON PlayerStats where
    parseJSON = withObject "PlayerStats" $ \v -> PlayerStats
        <$> v .:? "batting"
        <*> v .:? "pitching"

data BattingStats = BattingStats
    { bat_gamesPlayed           :: Maybe Int
    , bat_flyOuts               :: Maybe Int
    , bat_groundOuts            :: Maybe Int
    , bat_runs                  :: Maybe Int
    , bat_doubles               :: Maybe Int
    , bat_triples               :: Maybe Int
    , bat_homeRuns              :: Maybe Int
    , bat_strikeOuts            :: Maybe Int
    , bat_baseOnBalls           :: Maybe Int
    , bat_intentionalWalks      :: Maybe Int
    , bat_hits                  :: Maybe Int
    , bat_hitByPitch            :: Maybe Int
    , bat_atBats                :: Maybe Int
    , bat_caughtStealing        :: Maybe Int
    , bat_stolenBases           :: Maybe Int
    , bat_groundIntoDoublePlay  :: Maybe Int
    , bat_groundIntoTriplePlay  :: Maybe Int
    , bat_plateAppearances      :: Maybe Int
    , bat_totalBases            :: Maybe Int
    , bat_rbi                   :: Maybe Int
    , bat_leftOnBase            :: Maybe Int
    , bat_sacBunts              :: Maybe Int
    , bat_sacFlies              :: Maybe Int
    , bat_catchersInterference  :: Maybe Int
    , bat_pickoffs              :: Maybe Int
    } deriving (Show, Eq)

instance FromJSON BattingStats where
    parseJSON = withObject "BattingStats" $ \v -> BattingStats
        <$> v .:? "gamesPlayed"
        <*> v .:? "flyOuts"
        <*> v .:? "groundOuts"
        <*> v .:? "runs"
        <*> v .:? "doubles"
        <*> v .:? "triples"
        <*> v .:? "homeRuns"
        <*> v .:? "strikeOuts"
        <*> v .:? "baseOnBalls"
        <*> v .:? "intentionalWalks"
        <*> v .:? "hits"
        <*> v .:? "hitByPitch"
        <*> v .:? "atBats"
        <*> v .:? "caughtStealing"
        <*> v .:? "stolenBases"
        <*> v .:? "groundIntoDoublePlay"
        <*> v .:? "groundIntoTriplePlay"
        <*> v .:? "plateAppearances"
        <*> v .:? "totalBases"
        <*> v .:? "rbi"
        <*> v .:? "leftOnBase"
        <*> v .:? "sacBunts"
        <*> v .:? "sacFlies"
        <*> v .:? "catchersInterference"
        <*> v .:? "pickoffs"

data PitchingStats = PitchingStats
    { pit_gamesPlayed           :: Maybe Int
    , pit_gamesStarted          :: Maybe Int
    , pit_flyOuts               :: Maybe Int
    , pit_groundOuts            :: Maybe Int
    , pit_airOuts               :: Maybe Int
    , pit_runs                  :: Maybe Int
    , pit_doubles               :: Maybe Int
    , pit_triples               :: Maybe Int
    , pit_homeRuns              :: Maybe Int
    , pit_strikeOuts            :: Maybe Int
    , pit_baseOnBalls           :: Maybe Int
    , pit_intentionalWalks      :: Maybe Int
    , pit_hits                  :: Maybe Int
    , pit_hitByPitch            :: Maybe Int
    , pit_atBats                :: Maybe Int
    , pit_caughtStealing        :: Maybe Int
    , pit_stolenBases           :: Maybe Int
    , pit_numberOfPitches       :: Maybe Int
    , pit_inningsPitched        :: Maybe Text
    , pit_wins                  :: Maybe Int
    , pit_losses                :: Maybe Int
    , pit_saves                 :: Maybe Int
    , pit_saveOpportunities     :: Maybe Int
    , pit_holds                 :: Maybe Int
    , pit_blownSaves            :: Maybe Int
    , pit_earnedRuns            :: Maybe Int
    , pit_battersFaced          :: Maybe Int
    , pit_outs                  :: Maybe Int
    , pit_gamesPitched          :: Maybe Int
    , pit_completeGames         :: Maybe Int
    , pit_shutouts              :: Maybe Int
    , pit_pitchesThrown         :: Maybe Int
    , pit_balls                 :: Maybe Int
    , pit_strikes               :: Maybe Int
    , pit_hitBatsmen            :: Maybe Int
    , pit_balks                 :: Maybe Int
    , pit_wildPitches           :: Maybe Int
    , pit_pickoffs              :: Maybe Int
    , pit_rbi                   :: Maybe Int
    , pit_gamesFinished         :: Maybe Int
    , pit_inheritedRunners      :: Maybe Int
    , pit_inheritedRunnersScored:: Maybe Int
    , pit_catchersInterference  :: Maybe Int
    , pit_sacBunts              :: Maybe Int
    , pit_sacFlies              :: Maybe Int
    , pit_passedBall            :: Maybe Int
    } deriving (Show, Eq)

instance FromJSON PitchingStats where
    parseJSON = withObject "PitchingStats" $ \v -> PitchingStats
        <$> v .:? "gamesPlayed"
        <*> v .:? "gamesStarted"
        <*> v .:? "flyOuts"
        <*> v .:? "groundOuts"
        <*> v .:? "airOuts"
        <*> v .:? "runs"
        <*> v .:? "doubles"
        <*> v .:? "triples"
        <*> v .:? "homeRuns"
        <*> v .:? "strikeOuts"
        <*> v .:? "baseOnBalls"
        <*> v .:? "intentionalWalks"
        <*> v .:? "hits"
        <*> v .:? "hitByPitch"
        <*> v .:? "atBats"
        <*> v .:? "caughtStealing"
        <*> v .:? "stolenBases"
        <*> v .:? "numberOfPitches"
        <*> v .:? "inningsPitched"
        <*> v .:? "wins"
        <*> v .:? "losses"
        <*> v .:? "saves"
        <*> v .:? "saveOpportunities"
        <*> v .:? "holds"
        <*> v .:? "blownSaves"
        <*> v .:? "earnedRuns"
        <*> v .:? "battersFaced"
        <*> v .:? "outs"
        <*> v .:? "gamesPitched"
        <*> v .:? "completeGames"
        <*> v .:? "shutouts"
        <*> v .:? "pitchesThrown"
        <*> v .:? "balls"
        <*> v .:? "strikes"
        <*> v .:? "hitBatsmen"
        <*> v .:? "balks"
        <*> v .:? "wildPitches"
        <*> v .:? "pickoffs"
        <*> v .:? "rbi"
        <*> v .:? "gamesFinished"
        <*> v .:? "inheritedRunners"
        <*> v .:? "inheritedRunnersScored"
        <*> v .:? "catchersInterference"
        <*> v .:? "sacBunts"
        <*> v .:? "sacFlies"
        <*> v .:? "passedBall"

main :: IO ()
main = do
    jsonData <- B.readFile "testFiles/716896_boxscore.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    handPicked <- B.readFile "testFiles/shortened.json"
    let parsedResult = eitherDecodeStrict handPicked :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData