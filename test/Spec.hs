module Main (main) where

import Test.Hspec

import qualified Pelotero.Domain.PositionSpec   as Position
import qualified Pelotero.Domain.RosterSpec     as Roster
import qualified Pelotero.Domain.ScoringSpec    as Scoring
import qualified Pelotero.Domain.DraftSpec      as Draft
import qualified Pelotero.MLB.ConvertSpec       as Convert
import qualified Pelotero.Effects.PlayersSpec   as PlayersEff
import qualified Pelotero.ScoreSpec             as Score

main :: IO ()
main = hspec $ do
  Position.spec
  Roster.spec
  Scoring.spec
  Draft.spec
  Convert.spec
  PlayersEff.spec
  Score.spec