{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Newtype wrappers carrying the JSON encodings used to store
-- 'LeagueScoring', 'RosterLimits', and 'LineupLimits' in PostgreSQL
-- JSONB columns. The Domain types stay free of Aeson; this module
-- is the only place that bridges them to the JSON wire format.
--
-- Wrapping happens at the rel8 'Column' boundary in
-- 'Pelotero.DB.LeagueConfig'; the public 'LeagueConfigRow' API still
-- carries the unwrapped Domain types.
module Pelotero.DB.JsonB
  ( JsonbScoring (..)
  , JsonbRosterLimits (..)
  , JsonbLineupLimits (..)
  ) where

import           Data.Aeson         ( FromJSON (..), ToJSON (..)
                                    , object, withObject, (.:), (.=) )
import qualified Data.Aeson.Types   as Aeson
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text          as T

import qualified Rel8 as R

import           Pelotero.Domain.Roster
                     ( LineupLimits (..)
                     , RosterLimits (..)
                     , parseRosterSlot
                     , renderRosterSlot
                     )
import           Pelotero.Domain.Scoring
                     ( BattingMultipliers (..)
                     , LeagueScoring (..)
                     , PitchingMultipliers (..)
                     )

-- ---------------------------------------------------------------------
-- LeagueScoring
-- ---------------------------------------------------------------------

newtype JsonbScoring = JsonbScoring { unJsonbScoring :: LeagueScoring }
  deriving stock (Show, Eq)

-- | Internal-only wrapper so 'BattingMultipliers'\' field-by-field
-- encoding lives in this module rather than being inlined into
-- 'JsonbScoring'\'s instance.
newtype JsonbBattingMultipliers
  = JsonbBattingMultipliers BattingMultipliers

-- | Internal-only mirror for pitching.
newtype JsonbPitchingMultipliers
  = JsonbPitchingMultipliers PitchingMultipliers

instance ToJSON JsonbBattingMultipliers where
  toJSON (JsonbBattingMultipliers BattingMultipliers{..}) = object
    [ "single"          .= bmSingle
    , "double"          .= bmDouble
    , "triple"          .= bmTriple
    , "homeRun"         .= bmHomeRun
    , "rbi"             .= bmRbi
    , "run"             .= bmRun
    , "baseOnBalls"     .= bmBaseOnBalls
    , "stolenBase"      .= bmStolenBase
    , "hitByPitch"      .= bmHitByPitch
    , "strikeOut"       .= bmStrikeOut
    , "caughtStealing"  .= bmCaughtStealing
    ]

instance FromJSON JsonbBattingMultipliers where
  parseJSON = withObject "BattingMultipliers" $ \o ->
    fmap JsonbBattingMultipliers $ BattingMultipliers
      <$> o .: "single"
      <*> o .: "double"
      <*> o .: "triple"
      <*> o .: "homeRun"
      <*> o .: "rbi"
      <*> o .: "run"
      <*> o .: "baseOnBalls"
      <*> o .: "stolenBase"
      <*> o .: "hitByPitch"
      <*> o .: "strikeOut"
      <*> o .: "caughtStealing"

instance ToJSON JsonbPitchingMultipliers where
  toJSON (JsonbPitchingMultipliers PitchingMultipliers{..}) = object
    [ "win"           .= pmWin
    , "save"          .= pmSave
    , "qualityStart"  .= pmQualityStart
    , "inningPitched" .= pmInningPitched
    , "strikeOut"     .= pmStrikeOut
    , "completeGame"  .= pmCompleteGame
    , "shutout"       .= pmShutout
    , "baseOnBalls"   .= pmBaseOnBalls
    , "hitsAllowed"   .= pmHitsAllowed
    , "earnedRun"     .= pmEarnedRun
    , "hitBatsman"    .= pmHitBatsman
    , "loss"          .= pmLoss
    ]

instance FromJSON JsonbPitchingMultipliers where
  parseJSON = withObject "PitchingMultipliers" $ \o ->
    fmap JsonbPitchingMultipliers $ PitchingMultipliers
      <$> o .: "win"
      <*> o .: "save"
      <*> o .: "qualityStart"
      <*> o .: "inningPitched"
      <*> o .: "strikeOut"
      <*> o .: "completeGame"
      <*> o .: "shutout"
      <*> o .: "baseOnBalls"
      <*> o .: "hitsAllowed"
      <*> o .: "earnedRun"
      <*> o .: "hitBatsman"
      <*> o .: "loss"

instance ToJSON JsonbScoring where
  toJSON (JsonbScoring LeagueScoring{..}) = object
    [ "batting"  .= JsonbBattingMultipliers  lsBatting
    , "pitching" .= JsonbPitchingMultipliers lsPitching
    ]

instance FromJSON JsonbScoring where
  parseJSON = withObject "LeagueScoring" $ \o -> do
    JsonbBattingMultipliers  bm <- o .: "batting"
    JsonbPitchingMultipliers pm <- o .: "pitching"
    pure (JsonbScoring (LeagueScoring bm pm))

deriving via R.JSONBEncoded JsonbScoring instance R.DBType JsonbScoring

-- ---------------------------------------------------------------------
-- RosterLimits / LineupLimits
-- ---------------------------------------------------------------------

newtype JsonbRosterLimits = JsonbRosterLimits { unJsonbRosterLimits :: RosterLimits }
  deriving stock (Show, Eq)

newtype JsonbLineupLimits = JsonbLineupLimits { unJsonbLineupLimits :: LineupLimits }
  deriving stock (Show, Eq)

instance ToJSON JsonbRosterLimits where
  toJSON (JsonbRosterLimits (RosterLimits m)) =
    toJSON (Map.mapKeys renderRosterSlot m)

instance FromJSON JsonbRosterLimits where
  parseJSON v = do
    raw    <- parseJSON v :: Aeson.Parser (Map Text Int)
    parsed <- Map.fromList <$> traverse parsePair (Map.toList raw)
    pure (JsonbRosterLimits (RosterLimits parsed))
    where
      parsePair (k, n) = case parseRosterSlot k of
        Just s  -> pure (s, n)
        Nothing -> fail ("RosterLimits: unknown slot key " <> T.unpack k)

instance ToJSON JsonbLineupLimits where
  toJSON (JsonbLineupLimits (LineupLimits m)) =
    toJSON (Map.mapKeys renderRosterSlot m)

instance FromJSON JsonbLineupLimits where
  parseJSON v = do
    raw    <- parseJSON v :: Aeson.Parser (Map Text Int)
    parsed <- Map.fromList <$> traverse parsePair (Map.toList raw)
    pure (JsonbLineupLimits (LineupLimits parsed))
    where
      parsePair (k, n) = case parseRosterSlot k of
        Just s  -> pure (s, n)
        Nothing -> fail ("LineupLimits: unknown slot key " <> T.unpack k)

deriving via R.JSONBEncoded JsonbRosterLimits instance R.DBType JsonbRosterLimits
deriving via R.JSONBEncoded JsonbLineupLimits instance R.DBType JsonbLineupLimits