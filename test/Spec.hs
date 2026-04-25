module Main (main) where

import Test.Hspec (hspec, describe)

import qualified Pelotero.Domain.DraftSpec    as DraftSpec
import qualified Pelotero.Domain.PositionSpec as PositionSpec
import qualified Pelotero.Domain.RosterSpec   as RosterSpec
import qualified Pelotero.Domain.ScoringSpec  as ScoringSpec
import qualified Pelotero.MLB.ConvertSpec     as ConvertSpec

main :: IO ()
main = hspec $ do
  describe "Pelotero.Domain.Position" PositionSpec.spec
  describe "Pelotero.Domain.Roster"   RosterSpec.spec
  describe "Pelotero.Domain.Scoring"  ScoringSpec.spec
  describe "Pelotero.Domain.Draft"    DraftSpec.spec
  describe "Pelotero.MLB.Convert"     ConvertSpec.spec