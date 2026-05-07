module Main (main) where

import Test.Hspec

import qualified Pelotero.Domain.DraftSpec        as Draft
import qualified Pelotero.Domain.PlayerSpec       as DomainPlayer
import qualified Pelotero.Domain.PositionSpec     as Position
import qualified Pelotero.Domain.RosterSpec       as Roster
import qualified Pelotero.Domain.ScoringSpec      as Scoring
import qualified Pelotero.Effects.PlayersSpec     as PlayersEff
import qualified Pelotero.MLB.ConvertSpec         as Convert
import qualified Pelotero.Provider.ExternalIdSpec as ExternalId
import qualified Pelotero.ScoreSpec               as Score

main :: IO ()
main = hspec $ do
  -- Domain
  Position.spec
  Roster.spec
  Scoring.spec
  Draft.spec
  DomainPlayer.spec

  -- Provider
  ExternalId.spec

  -- Boundary
  Convert.spec

  -- Effects
  PlayersEff.spec

  -- Higher-level
  Score.spec