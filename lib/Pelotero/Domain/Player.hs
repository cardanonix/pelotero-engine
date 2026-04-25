-- | Domain representation of an MLB player. Kept separate from the wire
-- shape ("Pelotero.MLB.Wire.Player") so the domain can evolve independently
-- and the wire layer stays free of business logic.
module Pelotero.Domain.Player
  ( Player(..)
  , Handedness(..)
  , parseHandedness
  , renderHandedness
  ) where

import Data.Text (Text)

import Pelotero.Domain.Id (PlayerId, TeamId)
import Pelotero.Domain.Position (Position)

-- | A roster entry. Stable identity (PlayerId), denormalised display name,
-- current MLB team, primary position, and handedness for batting and
-- pitching. Active flag from upstream — keep it; we filter on it elsewhere.
data Player = Player
  { playerId        :: PlayerId
  , playerFirstName :: Text       -- ^ MLB's "useName"
  , playerLastName  :: Text       -- ^ MLB's "useLastName"
  , playerNameSlug  :: Text       -- ^ URL-safe identifier
  , playerTeamId    :: Maybe TeamId
  , playerPosition  :: Maybe Position
  , playerBatSide   :: Maybe Handedness
  , playerPitchHand :: Maybe Handedness
  , playerActive    :: Bool
  }
  deriving stock (Show, Eq)

-- | Batter or pitcher hand. MLB also reports "S" for switch-hitters.
data Handedness = LeftHanded | RightHanded | Switch
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Parse from MLB's single-letter code. "L"/"R"/"S" are well-formed;
-- anything else returns Nothing.
parseHandedness :: Text -> Maybe Handedness
parseHandedness = \case
  "L" -> Just LeftHanded
  "R" -> Just RightHanded
  "S" -> Just Switch
  _   -> Nothing

renderHandedness :: Handedness -> Text
renderHandedness = \case
  LeftHanded  -> "L"
  RightHanded -> "R"
  Switch      -> "S"