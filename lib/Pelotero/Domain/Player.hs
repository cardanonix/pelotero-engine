{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The domain 'Player' record and its 'Handedness' sum.
--
-- @parseHandedness@ accepts only @\"L\"@, @\"R\"@, @\"S\"@. @renderHandedness@
-- inverts that for human-facing display. @handCharййй@ is the total 'Char'
-- projection used everywhere persistence needs a single character (the DB
-- bat_side / pitch_hand columns store @TEXT@ but only ever a one-character
-- value; see @decision BatHandStoredAsText@ in @Pelotero.DB.Player@).
module Pelotero.Domain.Player
  ( Player (..)
  , Handedness (..)
  , parseHandedness
  , renderHandedness
  , handChar
  ) where

import           Data.Text             (Text)

import           Pelotero.Domain.Id    (PlayerId, TeamId)
import           Pelotero.Domain.Position (Position)

data Player = Player
  { playerId        :: !PlayerId
  , playerFirstName :: !Text       -- ^ MLB's "useName"
  , playerLastName  :: !Text       -- ^ MLB's "useLastName"
  , playerNameSlug  :: !Text       -- ^ URL-safe identifier
  , playerTeamId    :: !(Maybe TeamId)
  , playerPosition  :: !(Maybe Position)
  , playerBatSide   :: !(Maybe Handedness)
  , playerPitchHand :: !(Maybe Handedness)
  , playerActive    :: !Bool
  }
  deriving stock (Show, Eq)

data Handedness
  = LeftHanded
  | RightHanded
  | Switch
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Parse the wire-format single-character handedness code.
parseHandedness :: Text -> Maybe Handedness
parseHandedness = \case
  "L" -> Just LeftHanded
  "R" -> Just RightHanded
  "S" -> Just Switch
  _   -> Nothing

-- | Render handedness back to the canonical wire form.
renderHandedness :: Handedness -> Text
renderHandedness = \case
  LeftHanded  -> "L"
  RightHanded -> "R"
  Switch      -> "S"

-- | Total 'Char' projection of 'Handedness'.
--
-- Replaces the partial @T.head . renderHandedness@ idiom in sync code.
-- Adding a new 'Handedness' constructor without updating this function is
-- a @-Wincomplete-patterns@ error.
handChar :: Handedness -> Char
handChar = \case
  LeftHanded  -> 'L'
  RightHanded -> 'R'
  Switch      -> 'S'