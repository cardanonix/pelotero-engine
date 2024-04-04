module Main (main) where

import Control.Monad (filterM, forM, forM_)

import qualified Config as C
import Data.Aeson (FromJSON, ToJSON, encode, parseJSON, withObject, (.:))
import qualified Data.HashMap.Strict as HM
import Data.List (delete)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import qualified Input as I
import OfficialRoster as O
import qualified OfficialRoster as O
import qualified PlayerRanking as PR
import qualified Points as P
import qualified Roster as R
import System.Random (StdGen, newStdGen, randomR)
import Utility (
  computeChecksum,
  createRandomTeamID,
  generateRandomSHA256,
  readJson,
  shuffleList,
  writeJson,
 )
import Validators

-- Updated createRandomRankings function
createRandomRankings :: O.OfficialRoster -> IO [PR.PlayerRanking]
createRandomRankings officialRoster = do
  gen <- newStdGen
  let players = O.people officialRoster
  let (shuffledPlayers, _) = shuffleList players gen
  let rankings = zipWith (\rank player -> PR.PlayerRanking (O.playerId player) rank) [1 ..] shuffledPlayers
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

      currentTime <- getCurrentTime
      randomTeamId <- createRandomTeamID -- Use the new function here
      let shortTeamId = T.take 12 $ C.unwrapTeamID randomTeamId -- Unwrap here for filename
      let rankingData = PR.RankingData randomTeamId (T.pack "") currentTime rankings
      let rankingDataJson = encode rankingData
      let dataChecksum = computeChecksum rankingDataJson

      let rankingDataWithChecksum = rankingData{PR.dataChecksum = dataChecksum, PR.teamId = randomTeamId}

      let fileName = "testFiles/appData/rankings/_" ++ T.unpack shortTeamId ++ "_.json"
      writeJson fileName rankingDataWithChecksum

      putStrLn $ "Rankings written to JSON file successfully at " ++ fileName
    Left error ->
      putStrLn $ "Failed to load the roster: " ++ error