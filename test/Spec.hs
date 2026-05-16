module Main (main) where

import Test.Hspec

import qualified Pelotero.Domain.DraftSpec             as DomainDraft
import qualified Pelotero.Domain.PlayerSpec            as DomainPlayer
import qualified Pelotero.Domain.PositionSpec          as Position
import qualified Pelotero.Domain.RosterSpec            as Roster
import qualified Pelotero.Domain.ScoringSpec           as Scoring
import qualified Pelotero.Draft.MachineSpec            as DraftMachine
import qualified Pelotero.DraftSpec                    as Draft
import qualified Pelotero.Effects.PlayersSpec          as PlayersEff
import qualified Pelotero.MLB.ConvertSpec              as Convert
import qualified Pelotero.MLB.ConvertWarningsSpec      as ConvertWarnings
import qualified Pelotero.MLB.HistoricalBoxscoreSpec   as HistoricalBoxscore
import qualified Pelotero.Provider.ExternalIdSpec      as ExternalId
import qualified Pelotero.ScoreSpec                    as Score
import qualified Pelotero.Sync.BoxscoresSpec           as SyncBoxscores
import qualified Pelotero.Sync.PlayersSpec             as SyncPlayers
import qualified Pelotero.Sync.ScheduleSpec            as SyncSchedule

main :: IO ()
main = hspec $ do
  Position.spec
  Roster.spec
  Scoring.spec
  DomainDraft.spec
  DomainPlayer.spec

  ExternalId.spec

  Convert.spec
  ConvertWarnings.spec
  HistoricalBoxscore.spec

  Draft.spec
  DraftMachine.spec

  PlayersEff.spec

  Score.spec

  SyncPlayers.spec
  SyncSchedule.spec
  SyncBoxscores.spec