-- | Defensive positions in baseball. Exhaustive — MLB defines exactly these
-- ten codes. If the MLB API ships something we don't recognise, that's a
-- parse error at the wire boundary, not a silent fallback into 'Position'.
module Pelotero.Domain.Position
  ( Position(..)
  , parsePosition
  , renderPosition
  , isPitcher
  , isInfielder
  , isOutfielder
  ) where

import Data.Text (Text)

-- | A defensive position. Codes match MLB's official numbering for batting
-- positions, plus DH and a synthetic "Pitcher" cover (MLB distinguishes SP/RP
-- only via separate tables; the position itself is just \"P\").
data Position
  = Pitcher        -- ^ MLB code "1"  / scorer "P"
  | Catcher        -- ^ MLB code "2"  / scorer "C"
  | FirstBase      -- ^ MLB code "3"  / scorer "1B"
  | SecondBase     -- ^ MLB code "4"  / scorer "2B"
  | ThirdBase      -- ^ MLB code "5"  / scorer "3B"
  | Shortstop      -- ^ MLB code "6"  / scorer "SS"
  | LeftField      -- ^ MLB code "7"  / scorer "LF"
  | CenterField    -- ^ MLB code "8"  / scorer "CF"
  | RightField     -- ^ MLB code "9"  / scorer "RF"
  | DesignatedHitter -- ^ MLB code "10" / scorer "DH"
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Parse a position from MLB's coded representation. Accepts both the
-- numeric form ("1".."10") and the scorer form ("P", "C", "1B", ..., "DH").
-- "TWP" (two-way player, e.g. Ohtani's primary position) is intentionally not
-- handled here; it requires a different domain model and is out of scope for
-- Phase 1.
parsePosition :: Text -> Maybe Position
parsePosition = \case
  "1"  -> Just Pitcher
  "2"  -> Just Catcher
  "3"  -> Just FirstBase
  "4"  -> Just SecondBase
  "5"  -> Just ThirdBase
  "6"  -> Just Shortstop
  "7"  -> Just LeftField
  "8"  -> Just CenterField
  "9"  -> Just RightField
  "10" -> Just DesignatedHitter
  "P"  -> Just Pitcher
  "C"  -> Just Catcher
  "1B" -> Just FirstBase
  "2B" -> Just SecondBase
  "3B" -> Just ThirdBase
  "SS" -> Just Shortstop
  "LF" -> Just LeftField
  "CF" -> Just CenterField
  "RF" -> Just RightField
  "DH" -> Just DesignatedHitter
  _    -> Nothing

-- | Canonical text rendering — the scorer form. Matches what shows up on a
-- box score.
renderPosition :: Position -> Text
renderPosition = \case
  Pitcher          -> "P"
  Catcher          -> "C"
  FirstBase        -> "1B"
  SecondBase       -> "2B"
  ThirdBase        -> "3B"
  Shortstop        -> "SS"
  LeftField        -> "LF"
  CenterField      -> "CF"
  RightField       -> "RF"
  DesignatedHitter -> "DH"

isPitcher :: Position -> Bool
isPitcher Pitcher = True
isPitcher _       = False

isInfielder :: Position -> Bool
isInfielder = \case
  FirstBase  -> True
  SecondBase -> True
  ThirdBase  -> True
  Shortstop  -> True
  Catcher    -> True
  _          -> False

isOutfielder :: Position -> Bool
isOutfielder = \case
  LeftField   -> True
  CenterField -> True
  RightField  -> True
  _           -> False