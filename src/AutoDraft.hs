{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    Value,
    decode,
    eitherDecodeStrict,
    fromJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
 )
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sortOn, find)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShowM)
import System.Environment (getArgs)

import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R

-- Utility Functions
readJson :: FromJSON a => FilePath -> IO (Maybe a)
readJson filePath = decode <$> BL.readFile filePath

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

-- Main Drafting Logic
draftPlayers :: RankingData -> RankingData -> [OfficialPlayer] -> C.Configuration -> (R.Roster, R.Roster)
draftPlayers rankingData1 rankingData2 officialPlayers config = (finalRoster1, finalRoster2)
  where
    rankedPlayers1 = rankAndFilterPlayers (R.rankings rankingData1) officialPlayers
    rankedPlayers2 = rankAndFilterPlayers (R.rankings rankingData2) officialPlayers
    -- Example placeholder for distributing players; Implement according to your draft logic and config
    (finalRoster1, finalRoster2) = distributePlayers rankedPlayers1 rankedPlayers2 R.emptyRoster R.emptyRoster config


rankAndFilterPlayers :: [PlayerRanking] -> [OfficialPlayer] -> [(OfficialPlayer, Int)]
rankAndFilterPlayers playerRankings officialPlayers = mapMaybe mapRank officialPlayers
  where
    mapRank player = do
      ranking <- find (\r -> playerId r == opPlayerId player) playerRankings
      return (player, rank ranking)


-- You will need to implement logic to evenly distribute players according to their position and rankings
distributePlayers :: [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int)] -> R.Roster -> R.Roster -> C.Configuration -> (R.Roster, R.Roster)
distributePlayers rankedPlayers1 rankedPlayers2 initialRoster1 initialRoster2 config =
    foldl (distributePlayer config) (initialRoster1, initialRoster2, True) (mergePlayers rankedPlayers1 rankedPlayers2)
    where
        -- Merges two lists of players while maintaining their ranks and alternates between teams
        mergePlayers :: [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int, Bool)]
        mergePlayers xs ys = concatMap (\((p1, r1), (p2, r2)) -> [(p1, r1, True), (p2, r2, False)]) (zip xs ys)

        -- Adds a player to the appropriate roster based on the round-robin flag and checks position limits from config
        distributePlayer :: C.Configuration -> (R.Roster, R.Roster, Bool) -> (O.OfficialPlayer, Int, Bool) -> (R.Roster, R.Roster, Bool)
        distributePlayer cfg (roster1, roster2, toFirstTeam) (player, _, True)
            | toFirstTeam = (addToRoster cfg player roster1, roster2, not toFirstTeam)
            | otherwise   = (roster1, addToRoster cfg player roster2, not toFirstTeam)
        distributePlayer cfg (roster1, roster2, toFirstTeam) (player, _, False) = distributePlayer cfg (roster1, roster2, toFirstTeam) (player, 0, toFirstTeam)

emptyRoster :: Roster
emptyRoster = Roster [] [] [] [] [] [] [] [] []

addToRoster :: O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster player roster = case O.primaryPosition player of
  "C" -> roster { cR = useName player : cR roster }
  "1B" -> roster { b1R = useName player : b1R roster }
  "2B" -> roster { b2R = useName player : b2R roster }
  "3B" -> roster { b3R = useName player : b3R roster }
  "SS" -> roster { ssR = useName player : ssR roster }
  "OF" -> roster { ofR = useName player : ofR roster }
  "U" -> roster { uR = useName player : uR roster }
  "SP" -> roster { spR = useName player : spR roster }
  "RP" -> roster { rpR = useName player : rpR roster }
  _ -> roster -- we should handle unexpected positions or add more detailed logic

addToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster cfg player roster = 
    case O.primaryPosition player of
        "C"  -> addIfUnderLimit "C" (O.useName player) cfg roster
        "1B" -> addIfUnderLimit "1B" (O.useName player) cfg roster
        -- Continue for other positions
        _ -> roster  -- Consider handling unexpected positions

-- Utility function to check position limits and add a player if under limit
addIfUnderLimit :: Text -> O.OfficialPlayer -> C.Configuration -> R.Roster -> R.Roster
addIfUnderLimit position player cfg roster = case position of
    "C"  -> addPlayer "catcher" (O.useName player) cfg roster
    "1B" -> addPlayer "first" (O.useName player) cfg roster
    "2B" -> addPlayer "second" (O.useName player) cfg roster
    "3B" -> addPlayer "third" (O.useName player) cfg roster
    "SS" -> addPlayer "shortstop" (O.useName player) cfg roster
    "OF" -> addPlayer "outfield" (O.useName player) cfg roster
    "U"  -> addPlayer "utility" (O.useName player) cfg roster
    "SP" -> addPlayer "s_pitcher" (O.useName player) cfg roster
    "RP" -> addPlayer "r_pitcher" (O.useName player) cfg roster
    _    -> roster -- No action for unexpected positions

addPlayer :: Text -> Text -> C.Configuration -> R.Roster -> R.Roster
addPlayer posName playerName cfg roster = 
    let posLimit = lookupLimit posName (C.draftLimits cfg) -- You need to implement lookupLimit
        currentCount = countPlayers posName roster -- You need to implement countPlayers
    in if currentCount < posLimit
       then updateRoster posName playerName roster -- You need to implement updateRoster
       else roster

lookupLimit :: Text -> Configuration -> Int
lookupLimit posName cfg =
    let draftLimits = draft_limits (draft_parameters cfg)
    in case posName of
        "catcher"   -> dr_catcher draftLimits
        "first"     -> dr_first draftLimits
        "second"    -> dr_second draftLimits
        "third"     -> dr_third draftLimits
        "shortstop" -> dr_shortstop draftLimits
        "outfield"  -> dr_outfield draftLimits
        "utility"   -> dr_utility draftLimits
        "s_pitcher" -> dr_s_pitcher draftLimits
        "r_pitcher" -> dr_r_pitcher draftLimits
        _           -> 0  -- Default case for unhandled positions

-- Placeholder for countPlayers function
countPlayers :: Text -> R.Roster -> Int
countPlayers posName roster =
    case posName of
        "catcher"   -> length (R.cR roster)
        "first"     -> length (R.b1R roster)
        "second"    -> length (R.b2R roster)
        "third"     -> length (R.b3R roster)
        "shortstop" -> length (R.ssR roster)
        "outfield"  -> length (R.ofR roster)
        "utility"   -> length (R.uR roster)
        "s_pitcher" -> length (R.spR roster)
        "r_pitcher" -> length (R.rpR roster)
        _           -> 0

-- Placeholder for updateRoster function
updateRoster :: Text -> Text -> R.Roster -> R.Roster
updateRoster posName playerName roster =
    case posName of
        "catcher"   -> roster {R.cR = playerName : R.cR roster}
        "first"     -> roster {R.b1R = playerName : R.b1R roster}
        "second"    -> roster {R.b2R = playerName : R.b2R roster}
        "third"     -> roster {R.b3R = playerName : R.b3R roster}
        "shortstop" -> roster {R.ssR = playerName : R.ssR roster}
        "outfield"  -> roster {R.ofR = playerName : R.ofR roster}
        "utility"   -> roster {R.uR = playerName : R.uR roster}
        "s_pitcher" -> roster {R.spR = playerName : R.spR roster}
        "r_pitcher" -> roster {R.rpR = playerName : R.rpR roster}
        _           -> roster  -- No update for unhandled positions

-- Main Function
main :: IO ()
main = do
    -- Load and parse JSON data
    rankingData1 <- readJson "appData/head2head/team001_rankings.json" :: IO (Maybe RankingData)
    rankingData2 <- readJson "appData/head2head/team002_rankings.json" :: IO (Maybe RankingData)
    officialRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Maybe OfficialRoster)

    case (rankingData1, rankingData2, officialRoster) of
        (Just rd1, Just rd2, Just or) -> do
            let roster1 = draftPlayers (rankings rd1) (people or)
            let roster2 = draftPlayers (rankings rd2) (people or)
            writeJson "appData/draftedroster001.json" roster1
            writeJson "appData/draftedroster002.json" roster2
        _ -> putStrLn "Failed to load data"


-- main :: IO ()
-- main = do
--     let rankingOne = "appData/head2head/team001_rankings.json"
--     let rankingTwo = "appData/head2head/team002_rankings.json"
--         {- 
--                 data RankingData = RankingData
--                     { teamId        :: Text
--                     , dataChecksum  :: Text
--                     , lastUpdated   :: UTCTime
--                     , rankings      :: [PlayerRanking]  -- relevant data found here
--                     } deriving (Show, Eq, Generic)

--                 data PlayerRanking = PlayerRanking
--                     { playerId :: Int
--                     , rank     :: Int
--                     } deriving (Show, Eq, Generic) 
--         -}
--     let activeRoster = "appData/rosters/activePlayers.json"
--         {- 
--             data OfficialRoster = OfficialRoster
--                 { people :: [OfficialPlayer]
--                 , dataPulled :: Text
--                 , checksum :: Text
--                 }
--                 deriving (Show)

--             data OfficialPlayer = OfficialPlayer
--                 { playerId :: Int               -- relevant data here
--                 , useName :: Text
--                 , useLastName :: Text
--                 , nameSlug :: Text
--                 , currentTeam :: Int
--                 , primaryPosition :: Text       -- relevant data here
--                 , batSide :: Text
--                 , pitchHand :: Text
--                 , active :: Bool                -- relevant data here
--                 }
--                 deriving (Show, Eq) 
--         -}
--     draftConfig <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
--         {-       
--                 data DraftParameters = DraftParameters
--                     { autoDraft :: Bool
--                     , autoDraft_UTC :: Text
--                     , draft_limits :: DraftRoster -- relevant date found here
--                     }
--                     deriving (Show, Eq)   

--                 data DraftRoster = DraftRoster
--                     { dr_catcher :: Int
--                     , dr_first :: Int
--                     , dr_second :: Int
--                     , dr_third :: Int
--                     , dr_shortstop :: Int
--                     , dr_outfield :: Int
--                     , dr_utility :: Int
--                     , dr_s_pitcher :: Int
--                     , dr_r_pitcher :: Int
--                     }
--                     deriving (Show, Eq)
--         -}
--     -- using the relevant data, we then automatically fill the rosters with the players preventing cases of duplicate players on the rosters
--     draftedRosterOne :: IO (Either String R.Roster) -> writeJson "appData/draftedroster001.json" 
--     draftedRosterTwo :: IO (Either String R.Roster) -> writeJson "appData/draftedroster002.json" 
--         {- 
--                 -- LgManager ADT
--                 data LgManager = LgManager
--                     { status :: Text
--                     , commissioner :: Text
--                     , teamId :: Text
--                     , leagueID :: Text
--                     , current_lineup :: CurrentLineup
--                     , roster :: Roster
--                     }
--                     deriving (Show, Eq)

--                 data CurrentLineup = CurrentLineup
--                     { cC :: Text
--                     , b1C :: Text
--                     , b2C :: Text
--                     , b3C :: Text
--                     , ssC :: Text
--                     , ofC :: [Text]
--                     , uC :: Text
--                     , spC :: [Text]
--                     , rpC :: [Text]
--                     }
--                     deriving (Show, Eq)

--                 data Roster = Roster
--                     { cR :: [Text]
--                     , b1R :: [Text]
--                     , b2R :: [Text]
--                     , b3R :: [Text]
--                     , ssR :: [Text]
--                     , ofR :: [Text]
--                     , uR :: [Text]
--                     , spR :: [Text]
--                     , rpR :: [Text]
--                     }
--                     deriving (Show, Eq) 
--         -}

