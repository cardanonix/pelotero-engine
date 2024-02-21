{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Data.Maybe (mapMaybe)
import Data.List (find)
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified Ranking as PR
import Validators

readJson :: FromJSON a => FilePath -> IO (Maybe a)
readJson filePath = decode <$> BL.readFile filePath

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

-- Creates an empty roster based on the league's draft limits
emptyRoster :: R.Roster
emptyRoster = R.Roster [] [] [] [] [] [] [] [] []

draftPlayers :: [PR.PlayerRanking] -> [PR.PlayerRanking] -> [O.OfficialPlayer] -> C.Configuration -> IO (R.Roster, R.Roster)
draftPlayers rankings1 rankings2 officialPlayers config = do
    -- Convert rankings to a list of player IDs for easier processing
    let rankedPlayerIds1 = map PR.playerId rankings1
    let rankedPlayerIds2 = map PR.playerId rankings2
    let officialPlayerIds = map O.playerId officialPlayers

    -- Filter rankings to include only official players
    let validRankedIds1 = filter (`elem` officialPlayerIds) rankedPlayerIds1
    let validRankedIds2 = filter (`elem` officialPlayerIds) rankedPlayerIds2

    -- Draft players alternating between teams
    let draftCycle (roster1, roster2, availableIds) (rankId1, rankId2) = do
            let player1 = findPlayer rankId1 officialPlayers availableIds
            let player2 = findPlayer rankId2 officialPlayers (maybe availableIds (`delete` availableIds) player1)
            let updatedRoster1 = maybe roster1 (`addToRoster` roster1) player1
            let updatedRoster2 = maybe roster2 (`addToRoster` roster2) player2
            return (updatedRoster1, updatedRoster2, maybe availableIds (`delete` availableIds) player2)

    -- Fold over the paired rankings, drafting players
    foldM draftCycle (emptyRoster, emptyRoster, officialPlayerIds) (zip validRankedIds1 validRankedIds2)

-- Finds a player by ID in the list of official players, if available
findPlayer :: Int -> [O.OfficialPlayer] -> [Int] -> Maybe O.OfficialPlayer
findPlayer playerId players availableIds
    | playerId `elem` availableIds = find (\p -> O.playerId p == playerId) players
    | otherwise = Nothing

addToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster config player roster =
    let position = O.primaryPosition player
        rosterLimit = lookupLimit position (C.draft_limits $ C.draft_parameters config)
        currentPositionCount = countPlayers position roster
    in if currentPositionCount < rosterLimit
       then addPlayerToPosition position player roster
       else roster

lookupLimit :: Text.Text -> C.DraftRoster -> Int
lookupLimit position limits =
    case position of
        "catcher" -> dr_catcher limits
        "first" -> dr_first limits
        "second" -> dr_second limits
        "third" -> dr_third limits
        "shortstop" -> dr_shortstop limits
        "outfield" -> dr_outfield limits
        "utility" -> dr_utility limits
        "s_pitcher" -> dr_s_pitcher limits
        "r_pitcher" -> dr_r_pitcher limits
        _ -> 0

countPlayers :: Text.Text -> R.Roster -> Int
countPlayers position roster =
    case position of
        "catcher" -> length $ R.cR roster
        "first" -> length $ R.b1R roster
        "second" -> length $ R.b2R roster
        "third" -> length $ R.b3R roster
        "shortstop" -> length $ R.ssR roster
        "outfield" -> length $ R.ofR roster
        "utility" -> length $ R.uR roster
        "s_pitcher" -> length $ R.spR roster
        "r_pitcher" -> length $ R.rpR roster
        _ -> 0

addPlayerToPosition :: Text.Text -> O.OfficialPlayer -> R.Roster -> R.Roster
addPlayerToPosition position player roster =
    let playerIdText = Text.pack $ show $ O.playerId player
    in case position of
        "catcher" -> roster { R.cR = playerIdText : R.cR roster }
        "first" -> roster { R.b1R = playerIdText : R.b1R roster }
        "second" -> roster { R.b2R = playerIdText : R.b2R roster }
        "third" -> roster { R.b3R = playerIdText : R.b3R roster }
        "shortstop" -> roster { R.ssR = playerIdText : R.ssR roster }
        "outfield" -> roster { R.ofR = playerIdText : R.ofR roster }
        "utility" -> roster { R.uR = playerIdText : R.uR roster }
        "s_pitcher" -> roster { R.spR = playerIdText : R.spR roster }
        "r_pitcher" -> roster { R.rpR = playerIdText : R.rpR roster }
        _ -> roster  -- No action if position is not recognized

main :: IO ()
main = do
    rankings1 <- readJson "appData/head2head/team001_rankings.json"
    rankings2 <- readJson "appData/head2head/team002_rankings.json"
    officialPlayers <- readJson "appData/rosters/officialPlayers.json"
    config <- readJson "config/leagueConfig.json"

    case (rankings1, rankings2, officialPlayers, config) of
        (Just r1, Just r2, Just op, Just cfg) -> do
            (roster1, roster2) <- draftPlayers r1 r2 op cfg
            putStrLn "Draft completed."
            writeJson "draftResults/team1Roster.json" roster1
            writeJson "draftResults/team2Roster.json" roster2
        _ -> putStrLn "Error loading data."

{- 
data RankingData = RankingData
    { teamId        :: Text
    , dataChecksum  :: Text
    , lastUpdated   :: UTCTime
    , rankings      :: [PlayerRanking]
    } deriving (Show, Eq, Generic)


data PlayerRanking = PlayerRanking
    { playerId :: Int
    , rank     :: Int
    } deriving (Show, Eq, Generic)


data OfficialRoster = OfficialRoster
    { people :: [OfficialPlayer]
    , dataPulled :: Text
    , checksum :: Text
    }
    deriving (Show)

data OfficialPlayer = OfficialPlayer
    { playerId :: Int               -- relevant data here
    , useName :: Text
    , useLastName :: Text
    , nameSlug :: Text
    , currentTeam :: Int
    , primaryPosition :: Text       -- relevant data here
    , batSide :: Text
    , pitchHand :: Text
    , active :: Bool                -- relevant data here
    }
    deriving (Show, Eq) 
    
data DraftParameters = DraftParameters
    { autoDraft :: Bool
    , autoDraft_UTC :: Text
    , draft_limits :: DraftRoster -- relevant date found here
    }
    deriving (Show, Eq)   

data DraftRoster = DraftRoster
    { dr_catcher :: Int
    , dr_first :: Int
    , dr_second :: Int
    , dr_third :: Int
    , dr_shortstop :: Int
    , dr_outfield :: Int
    , dr_utility :: Int
    , dr_s_pitcher :: Int
    , dr_r_pitcher :: Int
    }
    deriving (Show, Eq)

data LgManager = LgManager
    { status :: Text
    , commissioner :: Text
    , teamId :: Text
    , leagueID :: Text
    , current_lineup :: CurrentLineup
    , roster :: Roster
    }
    deriving (Show, Eq)

data CurrentLineup = CurrentLineup
    { cC :: Text
    , b1C :: Text
    , b2C :: Text
    , b3C :: Text
    , ssC :: Text
    , ofC :: [Text]
    , uC :: Text
    , spC :: [Text]
    , rpC :: [Text]
    }
    deriving (Show, Eq)

data Roster = Roster
    { cR :: [Text]
    , b1R :: [Text]
    , b2R :: [Text]
    , b3R :: [Text]
    , ssR :: [Text]
    , ofR :: [Text]
    , uR :: [Text]
    , spR :: [Text]
    , rpR :: [Text]
    }
    deriving (Show, Eq) 
 -}