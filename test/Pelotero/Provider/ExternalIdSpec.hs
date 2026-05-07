{-# LANGUAGE OverloadedStrings #-}

module Pelotero.Provider.ExternalIdSpec (spec) where

import qualified Data.Text                 as T
import           Hedgehog                  (PropertyT, forAll, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import           Test.Hspec                (Spec, describe, it)
import           Test.Hspec.Hedgehog       (hedgehog)

import           Pelotero.Domain.Id
                     ( GameId (..)
                     , PlayerId (..)
                     , TeamId (..)
                     )
import           Pelotero.Provider.ExternalId
                     ( externalIdFromGameId
                     , externalIdFromPlayerId
                     , externalIdFromTeamId
                     , externalIdToGameId
                     , externalIdToPlayerId
                     , externalIdToTeamId
                     )

spec :: Spec
spec = describe "Pelotero.Provider.ExternalId" $ do

  describe "round-trip: toX . fromX === Just" $ do
    it "TeamId"   $ hedgehog prop_teamIdRoundtrip
    it "PlayerId" $ hedgehog prop_playerIdRoundtrip
    it "GameId"   $ hedgehog prop_gameIdRoundtrip

  describe "encoding matches legacy show form (DB compatibility)" $ do
    it "TeamId"   $ hedgehog prop_teamIdLegacyEncoding
    it "PlayerId" $ hedgehog prop_playerIdLegacyEncoding
    it "GameId"   $ hedgehog prop_gameIdLegacyEncoding

  describe "rejects garbage" $ do
    it "trailing characters fail" $ hedgehog prop_trailingFails
    it "leading whitespace fails" $ hedgehog prop_leadingWhitespaceFails
    it "empty string fails"       $ hedgehog $ do
      externalIdToTeamId   "" === Nothing
      externalIdToPlayerId "" === Nothing
      externalIdToGameId   "" === Nothing

prop_teamIdRoundtrip :: PropertyT IO ()
prop_teamIdRoundtrip = do
  n <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  externalIdToTeamId (externalIdFromTeamId (TeamId n)) === Just (TeamId n)

prop_playerIdRoundtrip :: PropertyT IO ()
prop_playerIdRoundtrip = do
  n <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  externalIdToPlayerId (externalIdFromPlayerId (PlayerId n))
    === Just (PlayerId n)

prop_gameIdRoundtrip :: PropertyT IO ()
prop_gameIdRoundtrip = do
  n <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  externalIdToGameId (externalIdFromGameId (GameId n)) === Just (GameId n)

prop_teamIdLegacyEncoding :: PropertyT IO ()
prop_teamIdLegacyEncoding = do
  n <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  externalIdFromTeamId (TeamId n) === T.pack (show n)

prop_playerIdLegacyEncoding :: PropertyT IO ()
prop_playerIdLegacyEncoding = do
  n <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  externalIdFromPlayerId (PlayerId n) === T.pack (show n)

prop_gameIdLegacyEncoding :: PropertyT IO ()
prop_gameIdLegacyEncoding = do
  n <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  externalIdFromGameId (GameId n) === T.pack (show n)

prop_trailingFails :: PropertyT IO ()
prop_trailingFails = do
  n    <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  junk <- forAll $ Gen.text (Range.linear 1 8) Gen.alpha
  let bad = T.pack (show n) <> junk
  externalIdToTeamId   bad === Nothing
  externalIdToPlayerId bad === Nothing
  externalIdToGameId   bad === Nothing

prop_leadingWhitespaceFails :: PropertyT IO ()
prop_leadingWhitespaceFails = do
  n <- forAll $ Gen.int (Range.linearFrom 0 minBound maxBound)
  let bad = " " <> T.pack (show n)
  externalIdToTeamId   bad === Nothing
  externalIdToPlayerId bad === Nothing
  externalIdToGameId   bad === Nothing