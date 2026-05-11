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
import qualified IntegrationTest.SyncBoxscoresSpec       as SyncBoxscoresSpec
import qualified IntegrationTest.SyncPlayersSpec         as SyncPlayersSpec
import qualified IntegrationTest.SyncScheduleSpec        as SyncScheduleSpec
import qualified IntegrationTest.TeamRepoSpec            as TeamRepoSpec

import           IntegrationTest.Setup                   (withTestPool)

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
  SyncScheduleSpec.spec
  SyncBoxscoresSpec.spec
  ScoreSpec.spec
  SmokeSpec.spec