module Main (main) where

import Test.Hspec (hspec)

import qualified IntegrationTest.TeamRepoSpec          as TeamRepoSpec
import qualified IntegrationTest.PlayerRepoSpec        as PlayerRepoSpec
import qualified IntegrationTest.FetchLogRepoSpec      as FetchLogRepoSpec
import qualified IntegrationTest.GameRepoSpec          as GameRepoSpec
import qualified IntegrationTest.BoxscoreEntryRepoSpec as BoxscoreEntryRepoSpec

main :: IO ()
main = hspec $ do
  TeamRepoSpec.spec
  PlayerRepoSpec.spec
  FetchLogRepoSpec.spec
  GameRepoSpec.spec
  BoxscoreEntryRepoSpec.spec