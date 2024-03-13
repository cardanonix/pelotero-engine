
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
    ( computeChecksum,
      readJson,
      shuffleList,
      writeJson,
      generateRandomSHA256 )

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
      randomTeamId <- generateRandomSHA256  -- Generate a random SHA256 hash for the teamId

      -- Use only the first 10 characters of the randomTeamId for the filename
      let shortTeamId = T.take 10 randomTeamId

      -- Serialize rankingData to JSON for checksum calculation
      let rankingData = PR.RankingData randomTeamId (T.pack "") currentTime rankings
      let rankingDataJson = encode rankingData
      let dataChecksum = computeChecksum rankingDataJson

      -- Update rankingData with actual checksum and random teamId
      let rankingDataWithChecksum = rankingData { 
        PR.dataChecksum = dataChecksum, PR.teamId = randomTeamId 
        }

      -- Write the ranking data to a file, using shortTeamId in the filename
      let fileName = "testFiles/appData/rankings/" ++ T.unpack shortTeamId ++ ".json"
      writeJson fileName rankingDataWithChecksum

      putStrLn $ "Rankings written to JSON file successfully at " ++ fileName
    Left error ->
      putStrLn $ "Failed to load the roster: " ++ error
