
module Main (main) where

import Control.Monad ( filterM, forM, forM_)

import System.Random (randomR, newStdGen, StdGen)
import Data.List (delete)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, parseJSON, withObject, (.:))
import qualified Data.HashMap.Strict as HM
import qualified Config as C
import qualified Input as I
import OfficialRoster as O
import qualified OfficialRoster as O
import qualified Points as P
import qualified Roster as R
import qualified Ranking as PR
import Validators
import Utility
    ( readJson, writeJson, computeChecksum, shuffleList )


-- Updated createRandomRankings function
createRandomRankings :: O.OfficialRoster -> IO [PR.PlayerRanking]
createRandomRankings officialRoster = do
  gen <- newStdGen
  let players = O.people officialRoster
  let (shuffledPlayers, _) = shuffleList players gen
  let rankings = zipWith (\rank player -> PR.PlayerRanking (O.playerId player) rank) [1..] shuffledPlayers
  return rankings



main :: IO ()
main = do
    eitherRoster <- readJson "testFiles/appData/rosters/activePlayers.json"
    case eitherRoster of
        Right roster -> do
            rankings <- createRandomRankings roster
            putStrLn "Randomly generated player rankings:"
            forM_ rankings $ \(PR.PlayerRanking playerId rank) ->
                putStrLn $ "Player ID: " ++ show playerId ++ ", Rank: " ++ show rank

            -- Generate the necessary values for RankingData
            currentTime <- getCurrentTime
            let teamId = T.pack "yourTeamId"  -- Convert String to Text

            -- Serialize rankingData to JSON for checksum calculation
            let rankingData = PR.RankingData teamId (T.pack "") currentTime rankings  -- Temporarily empty checksum, converted to Text
            let rankingDataJson = encode rankingData
            let dataChecksum = computeChecksum rankingDataJson

            -- Update rankingData with actual checksum
            let rankingDataWithChecksum = rankingData { PR.dataChecksum = dataChecksum }

            -- Write the ranking data to a file
            writeJson "testFiles/appData/rankings/randomRankings.json" rankingDataWithChecksum

            putStrLn "Rankings written to JSON file successfully."
        Left error ->
            putStrLn $ "Failed to load the roster: " ++ error
