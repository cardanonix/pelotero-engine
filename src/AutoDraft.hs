{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Data.Maybe (mapMaybe)
import Data.List (find)
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified Ranking as PR

-- Utility Functions
readJson :: FromJSON a => FilePath -> IO (Maybe a)
readJson filePath = decode <$> BL.readFile filePath

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

draftPlayers :: PR.RankingData -> PR.RankingData -> [O.OfficialPlayer] -> C.Configuration -> (R.Roster, R.Roster)
draftPlayers rankingData1 rankingData2 officialPlayers config = 
  let
    rankedPlayers1 = rankAndFilterPlayers (PR.rankings rankingData1) officialPlayers
    rankedPlayers2 = rankAndFilterPlayers (PR.rankings rankingData2) officialPlayers
    mergedPlayers = mergePlayers rankedPlayers1 rankedPlayers2
  in
    distributePlayers mergedPlayers emptyRoster emptyRoster config

rankAndFilterPlayers :: [PR.PlayerRanking] -> [O.OfficialPlayer] -> [(O.OfficialPlayer, Int)]
rankAndFilterPlayers playerRankings officialPlayers = mapMaybe mapRank officialPlayers
  where
    mapRank player = do
      ranking <- find (\r -> PR.playerId r == O.playerId player) playerRankings
      return (player, PR.rank ranking)

distributePlayers :: [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int)] -> R.Roster -> R.Roster -> C.Configuration -> (R.Roster, R.Roster)
distributePlayers rankedPlayers1 rankedPlayers2 initialRoster1 initialRoster2 config =
    foldl (distributePlayers config) (initialRoster1, initialRoster2, True) (mergePlayers rankedPlayers1 rankedPlayers2)

emptyRoster :: R.Roster
emptyRoster = R.Roster [] [] [] [] [] [] [] [] []

addToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster cfg player roster =
    let playerIdStr = show $ O.playerId player  -- Convert player ID to String
    in if not (playerIdExists playerIdStr roster)
       then addIfUnderLimit (O.primaryPosition player) playerIdStr cfg roster
       else roster

playerIdExists :: String -> R.Roster -> Bool
playerIdExists pid roster =
    pid `elem` concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]

addIfUnderLimit :: Text -> String -> C.Configuration -> R.Roster -> R.Roster
addIfUnderLimit position playerId cfg roster = 
    let posLimit = lookupLimit position cfg
        currentCount = countPlayers position roster
    in if currentCount < posLimit
       then updateRoster position playerId roster
       else roster

mergePlayers :: [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int)]
mergePlayers xs ys = concatMap (\((x, rx), (y, ry)) -> [(x, rx), (y, ry)]) $ zip xs ys

addPlayer :: Text -> Text -> C.Configuration -> R.Roster -> R.Roster
addPlayer posName playerName cfg roster = 
    let posLimit = lookupLimit posName cfg
        currentCount = countPlayers posName roster 
    in if currentCount < posLimit
       then updateRoster posName playerName roster
       else roster


lookupLimit :: Text -> C.Configuration -> Int
lookupLimit posName cfg = 
  let limits = C.draft_parameters cfg
      draftLimits = C.draft_limits limits 
  in case posName of
      "catcher"   -> C.dr_catcher draftLimits
      "first"     -> C.dr_first draftLimits
      "second"    -> C.dr_second draftLimits
      "third"     -> C.dr_third draftLimits
      "shortstop" -> C.dr_shortstop draftLimits
      "outfield"  -> C.dr_outfield draftLimits
      "utility"   -> C.dr_utility draftLimits
      "s_pitcher" -> C.dr_s_pitcher draftLimits
      "r_pitcher" -> C.dr_r_pitcher draftLimits
      _           -> 0  -- Default case for unhandled positions      

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

updateRoster :: Text -> String -> R.Roster -> R.Roster
updateRoster posName playerId roster =
    case posName of
        "C"  -> roster {R.cR = addUnique playerId (R.cR roster)}
        "1B" -> roster {R.b1R = addUnique playerId (R.b1R roster)}
        "2B" -> roster {R.b2R = addUnique playerId (R.b2R roster)}
        "3B" -> roster {R.b3R = addUnique playerId (R.b3R roster)}
        "SS" -> roster {R.ssR = addUnique playerId (R.ssR roster)}
        "OF" -> roster {R.ofR = addUnique playerId (R.ofR roster)}
        "U"  -> roster {R.uR = addUnique playerId (R.uR roster)}
        "SP" -> roster {R.spR = addUnique playerId (R.spR roster)}
        "RP" -> roster {R.rpR = addUnique playerId (R.rpR roster)}
        _    -> roster  -- No update for unhandled positions

addUnique :: String -> [String] -> [String]
addUnique pid pids = if pid `elem` pids then pids else pid : pids

main :: IO ()
main = do
  rankingData1 <- readJson "appData/head2head/team001_rankings.json" :: IO (Maybe PR.RankingData)
  rankingData2 <- readJson "appData/head2head/team002_rankings.json" :: IO (Maybe PR.RankingData)
  officialRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Maybe O.OfficialRoster)
  config <- readJson "path/to/config.json" :: IO (Maybe C.Configuration)

  case (rankingData1, rankingData2, officialRoster, config) of
    (Just rd1, Just rd2, Just or, Just cfg) -> do
      let (roster1, roster2) = draftPlayers rd1 rd2 (O.people or) cfg
      writeJson "appData/draftedroster001.json" roster1
      writeJson "appData/draftedroster002.json" roster2
    _ -> putStrLn "Failed to load data"

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