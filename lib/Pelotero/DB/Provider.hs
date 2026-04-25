-- | Identity providers — the upstream sources we pull data from. The DB
-- stores provider names as free-form TEXT; this module is the application's
-- canonical list of which provider names are legitimate. Code that writes
-- to 'player_external_id' (or its sibling tables) goes through this type;
-- code that reads tolerates unknown names so a future provider can ship in
-- one deploy without breaking older readers.
module Pelotero.DB.Provider
  ( ProviderName(..)
  , renderProviderName
  , parseProviderName
  ) where

import Data.Text (Text)

-- | The set of providers we currently sync from. Adding one is intentionally
-- a code change: it forces us to enumerate which sync code path produces it
-- and which tables consume it.
data ProviderName
  = -- | MLB Stats API. Today's primary source.
    ProviderMLB
  | -- | Internal records — minor-league call-ups we know about before MLB
    -- assigns an id, retired players we score in historical leagues, etc.
    ProviderInternal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | The wire-format name. Stable across releases — changing one of these is
-- a data migration.
renderProviderName :: ProviderName -> Text
renderProviderName = \case
  ProviderMLB      -> "mlb"
  ProviderInternal -> "pelotero-internal"

-- | Tolerant parser: returns 'Nothing' for unknown names rather than failing
-- the whole read. Callers that don't care about the exact provider can fall
-- back to displaying the raw text.
parseProviderName :: Text -> Maybe ProviderName
parseProviderName = \case
  "mlb"               -> Just ProviderMLB
  "pelotero-internal" -> Just ProviderInternal
  _                   -> Nothing