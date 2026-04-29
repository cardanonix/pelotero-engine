module Main (main) where

import Test.Hspec (hspec)

import qualified IntegrationTest.TeamRepoSpec          as TeamRepoSpec
import qualified IntegrationTest.PlayerRepoSpec        as PlayerRepoSpec
import qualified IntegrationTest.FetchLogRepoSpec      as FetchLogRepoSpec
import qualified IntegrationTest.GameRepoSpec          as GameRepoSpec
import qualified IntegrationTest.BoxscoreEntryRepoSpec as BoxscoreEntryRepoSpec
import qualified IntegrationTest.RosterSlotRepoSpec    as RosterSlotRepoSpec
import qualified IntegrationTest.LineupSlotRepoSpec    as LineupSlotRepoSpec
import qualified IntegrationTest.LeagueConfigRepoSpec  as LeagueConfigRepoSpec
import qualified IntegrationTest.LeagueTeamRepoSpec    as LeagueTeamRepoSpec
import qualified IntegrationTest.PlayerRankingRepoSpec as PlayerRankingRepoSpec
import qualified IntegrationTest.DraftPickRepoSpec     as DraftPickRepoSpec
import qualified IntegrationTest.SyncPlayersSpec       as SyncPlayersSpec

main :: IO ()
main = hspec $ do
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