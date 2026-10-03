module Main (main) where

import Test.Hspec

import qualified Pelotero.Domain.DraftSpec        as DomainDraft
import qualified Pelotero.Domain.PlayerSpec       as DomainPlayer
import qualified Pelotero.Domain.PositionSpec     as Position
import qualified Pelotero.Domain.RosterSpec       as Roster
import qualified Pelotero.Domain.ScoringSpec      as Scoring
import qualified Pelotero.Domain.EligibilitySpec  as Eligibility
import qualified Pelotero.Draft.MachineSpec       as DraftMachine
import qualified Pelotero.Draft.RunSpec           as DraftRun
import qualified Pelotero.DraftSpec               as Draft
import qualified Pelotero.Effects.PlayersSpec     as PlayersEff
import qualified Pelotero.Effects.RandomSpec      as RandomEff
import qualified Pelotero.League.SetupSpec        as LeagueSetup
import qualified Pelotero.MatchupSpec             as Matchup
import qualified Pelotero.MLB.ConvertSpec         as Convert
import qualified Pelotero.Provider.ExternalIdSpec as ExternalId
import qualified Pelotero.ScoreSpec               as Score
import qualified Pelotero.Sync.BoxscoresSpec      as SyncBoxscores
import qualified Pelotero.Sync.PlayersSpec        as SyncPlayers
import qualified Pelotero.Sync.ScheduleSpec       as SyncSchedule

main :: IO ()
main = hspec $ do
  Position.spec
  Roster.spec
  Scoring.spec
  DomainDraft.spec
  DomainPlayer.spec
  Eligibility.spec

  ExternalId.spec

  Convert.spec

  Draft.spec
  DraftMachine.spec
  DraftRun.spec

  PlayersEff.spec
  RandomEff.spec

  Score.spec
  Matchup.spec
  LeagueSetup.spec

  SyncPlayers.spec
  SyncSchedule.spec
  SyncBoxscores.spec