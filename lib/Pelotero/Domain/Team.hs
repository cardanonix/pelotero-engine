-- | Domain representation of an MLB team. We track only the identity and
-- enough denormalised text to render lineups and box scores; we don't pull in
-- venues, divisions, or league affiliations at this stage.
module Pelotero.Domain.Team
  ( Team(..)
  ) where

import Data.Text (Text)

import Pelotero.Domain.Id (TeamId)

data Team = Team
  { teamId           :: TeamId
  , teamName         :: Text  -- ^ "Houston Astros"
  , teamAbbreviation :: Text  -- ^ "HOU"
  , teamLocationName :: Text  -- ^ "Houston"
  }
  deriving stock (Show, Eq)