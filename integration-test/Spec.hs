module Main (main) where

import           Test.Hspec                              (hspec, aroundAll)

import qualified IntegrationTest.BoxscoreEntryRepoSpec   as BoxscoreEntryRepoSpec
import qualified IntegrationTest.DraftPickRepoSpec       as DraftPickRepoSpec
import qualified IntegrationTest.FetchLogRepoSpec        as FetchLogRepoSpec
import qualified IntegrationTest.GameRepoSpec            as GameRepoSpec
import qualified IntegrationTest.LeagueConfigRepoSpec    as LeagueConfigRepoSpec
import qualified IntegrationTest.LeagueTeamRepoSpec      as LeagueTeamRepoSpec
import qualified IntegrationTest.LineupSlotRepoSpec      as LineupSlotRepoSpec
import qualified IntegrationTest.PlayerRankingRepoSpec   as PlayerRankingRepoSpec
import qualified IntegrationTest.PlayerRepoSpec          as PlayerRepoSpec
import qualified IntegrationTest.RosterSlotRepoSpec      as RosterSlotRepoSpec
import qualified IntegrationTest.ScoreSpec               as ScoreSpec
import qualified IntegrationTest.SmokeSpec               as SmokeSpec
import qualified IntegrationTest.SyncPlayersSpec         as SyncPlayersSpec
import qualified IntegrationTest.TeamRepoSpec            as TeamRepoSpec

import           IntegrationTest.Setup                   (withTestPool)

-- | One pool, shared across the entire suite via 'aroundAll'. Each
-- repo test still uses 'runRolledBack' inside, so the pool is reused
-- but per-test data isolation is preserved. Effect-interpreter specs
-- (Score, Smoke, SyncPlayers) call 'cleanDatabase' explicitly when
-- they need to commit across multiple transactions.
main :: IO ()
main = hspec $ aroundAll withTestPool $ do
  TeamRepoSpec.spec
  PlayerRepoSpec.spec
  FetchLogRepoSpec.spec
  GameRepoSpec.spec
  BoxscoreEntryRepoSpec.spec
  RosterSlotRepoSpec.spec
  LineupSlotRepoSpec.spec
  LeagueConfigRepoSpec.spec
  LeagueTeamRepoSpec.spec
  PlayerRankingRepoSpec.spec
  DraftPickRepoSpec.spec
  SyncPlayersSpec.spec
  ScoreSpec.spec
  SmokeSpec.spec