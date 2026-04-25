-- | Wire-to-domain conversion for MLB API responses. The wire types
-- ("Pelotero.MLB.Wire.*") parse the upstream JSON; this module turns them
-- into the cleaner domain types ("Pelotero.Domain.*"), discarding fields we
-- don't model and validating identifiers.
--
-- Conversion is lenient: when the upstream payload is malformed (e.g. an
-- unknown position code, a player ID of zero), we emit a 'ConvertWarning'
-- and substitute a sensible default. The caller decides whether to log,
-- ignore, or escalate. In production we'll wire warnings into katip
-- (Phase 3); for now they're plain values.
--
-- Why not fail-fast? The MLB feed ships partial records constantly —
-- spring-training rosters with no team, two-way players coded as "TWP",
-- pitcher batting lines from rare AL pitcher PA's. Failing the whole sync
-- because one record is shaped oddly would be operationally hostile.
module Pelotero.MLB.Convert
  ( -- * Conversion
    convertPlayer
  , convertPlayers
  , convertSchedule
  , convertBoxscore
    -- * Reporting
  , ConvertWarning(..)
  , renderWarning
  , logWarnings
    -- * Re-exports for convenience
  , BoxscoreEntry(..)
  ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import System.IO (Handle, hPutStrLn, stderr)
import qualified Data.Text as T
import System.IO (Handle, hPutStrLn, stderr)

import Pelotero.Domain.Game
  ( Game(..)
  , GameSchedule(..)
  )
import Pelotero.Domain.Id
  ( GameId(..)
  , PlayerId(..)
  , TeamId(..)
  )
import Pelotero.Domain.Player
  ( Handedness
  , Player(..)
  , parseHandedness
  )
import Pelotero.Domain.Position (Position, parsePosition)
import Pelotero.Domain.Stats
  ( BattingStats(..)
  , PitchingStats(..)
  , emptyBatting
  , emptyPitching
  )
import qualified Pelotero.MLB.Wire.Boxscore as WB
import qualified Pelotero.MLB.Wire.Player as WP
import qualified Pelotero.MLB.Wire.Schedule as WS

--------------------------------------------------------------------------------
-- Warnings

-- | Things the converter encountered that weren't fatal but the operator
-- probably wants to know about. Designed to be cheap to construct and trivial
-- to render; we'll attach more structured context (trace IDs, request IDs) in
-- Phase 3 once the effects layer is in place.
data ConvertWarning
  = -- | Player record had an ID of @0@ or negative, which MLB never legitimately
    -- emits. The whole record is dropped.
    InvalidPlayerId Int
  | -- | Position code didn't match any of the ten we recognise. The player is
    -- still kept, but with @playerPosition = Nothing@.
    UnknownPosition !Int !Text
  | -- | Bat/pitch handedness wasn't \"L\"/\"R\"/\"S\". Kept with 'Nothing'.
    UnknownHandedness !Int !Text
  | -- | Schedule entry had an unparseable date. Whole entry is dropped.
    InvalidGameDate !Int !Text
  | -- | Schedule entry was missing a team reference. Whole game is dropped.
    MissingTeamRef !Int
  deriving stock (Show, Eq)

-- | Single-line, human-readable rendering for log destinations.
renderWarning :: ConvertWarning -> Text
renderWarning = \case
  InvalidPlayerId pid ->
    "convert: dropped player with invalid id " <> tshow pid
  UnknownPosition pid code ->
    "convert: player " <> tshow pid
      <> " has unknown position code " <> T.pack (show code)
      <> "; setting Nothing"
  UnknownHandedness pid code ->
    "convert: player " <> tshow pid
      <> " has unknown hand code " <> T.pack (show code)
      <> "; setting Nothing"
  InvalidGameDate gid raw ->
    "convert: dropped game " <> tshow gid
      <> " with unparseable date " <> T.pack (show raw)
  MissingTeamRef gid ->
    "convert: dropped game " <> tshow gid <> " missing team reference"

-- | Default sink: render each warning to stderr. Returns immediately if the
-- list is empty so it's safe to call unconditionally.
logWarnings :: [ConvertWarning] -> IO ()
logWarnings = logWarningsTo stderr

-- | Variant that targets an arbitrary 'Handle'. Used by the test suite to
-- capture warnings into a buffer.
logWarningsTo :: Handle -> [ConvertWarning] -> IO ()
logWarningsTo h ws = unless (null ws) $
  mapM_ (\w -> hPutStrLn h (T.unpack (renderWarning w))) ws

--------------------------------------------------------------------------------
-- Players

-- | Convert one wire player. Returns 'Nothing' for records we refuse to admit
-- (currently: @id <= 0@). Warnings collected via the @Writer@-shaped tuple.
convertPlayer :: WP.WirePlayer -> ([ConvertWarning], Maybe Player)
convertPlayer wp
  | WP.wpId wp <= 0 =
      ([InvalidPlayerId (WP.wpId wp)], Nothing)
  | otherwise =
      let pid = WP.wpId wp

          (posWarn, position) = convertPosition pid (WP.wpPrimaryPosition wp)
          (batWarn, batSide)  = convertHand pid (WP.wpBatSide wp)
          (pitWarn, pitchHnd) = convertHand pid (WP.wpPitchHand wp)

          player = Player
            { playerId        = PlayerId pid
            , playerFirstName = orEmpty (WP.wpUseName wp)
            , playerLastName  = orEmpty (WP.wpUseLastName wp)
            , playerNameSlug  = orEmpty (WP.wpNameSlug wp)
            , playerTeamId    = TeamId . WP.wtrId <$> WP.wpCurrentTeam wp
            , playerPosition  = position
            , playerBatSide   = batSide
            , playerPitchHand = pitchHnd
            , playerActive    = WP.wpActive wp
            }
      in (posWarn <> batWarn <> pitWarn, Just player)

-- | Convert a roster envelope. Warnings from each record are concatenated;
-- order is preserved so callers can correlate by position in the output list.
convertPlayers :: WP.WirePlayerEnvelope -> ([ConvertWarning], [Player])
convertPlayers env =
  let results = map convertPlayer (WP.wirePlayers env)
      warns   = concatMap fst results
      players = mapMaybe snd results
  in (warns, players)

convertPosition
  :: Int
  -> Maybe WP.WirePositionRef
  -> ([ConvertWarning], Maybe Position)
convertPosition _   Nothing  = ([], Nothing)
convertPosition pid (Just r) =
  -- Prefer the abbreviation when present (it's the scorer form, "1B"/"DH"),
  -- fall back to the numeric code, and finally give up with a warning.
  case (WP.wprAbbreviation r, WP.wprCode r) of
    (Just abbr, _) | Just p <- parsePosition abbr -> ([], Just p)
    (_, Just code) | Just p <- parsePosition code -> ([], Just p)
    (Just abbr, _) -> ([UnknownPosition pid abbr], Nothing)
    (_, Just code) -> ([UnknownPosition pid code], Nothing)
    _              -> ([], Nothing)

convertHand
  :: Int
  -> Maybe WP.WireHandRef
  -> ([ConvertWarning], Maybe Handedness)
convertHand _   Nothing                            = ([], Nothing)
convertHand _   (Just (WP.WireHandRef Nothing))    = ([], Nothing)
convertHand pid (Just (WP.WireHandRef (Just code))) =
  case parseHandedness code of
    Just h  -> ([], Just h)
    Nothing -> ([UnknownHandedness pid code], Nothing)

--------------------------------------------------------------------------------
-- Schedule

-- | Flatten a wire schedule envelope into a domain 'GameSchedule'. Per-game
-- failures (bad date, missing team) are reported as warnings and the game is
-- dropped; we never raise an exception.
convertSchedule :: WS.WireScheduleEnvelope -> ([ConvertWarning], GameSchedule)
convertSchedule env =
  let (warns, games) = foldr step ([], []) (WS.wseDates env)
  in (warns, GameSchedule games)
  where
    step entry (ws, gs) =
      let (newWs, newGs) = convertDateEntry entry
      in (newWs <> ws, newGs <> gs)

convertDateEntry :: WS.WireDateEntry -> ([ConvertWarning], [Game])
convertDateEntry de =
  case parseDate (WS.wdeDate de) of
    Nothing  -> ([], [])  -- empty/bad dates with no games aren't worth warning about
    Just day ->
      let games = maybe [] id (WS.wdeGames de)
          results = map (convertGame day) games
      in (concatMap fst results, mapMaybe snd results)

convertGame :: Day -> WS.WireGame -> ([ConvertWarning], Maybe Game)
convertGame day wg =
  let gid = WS.wgGamePk wg
      teams = WS.wgTeams wg
      maway = teams >>= WS.wgtAway >>= WS.wgtTeamId
      mhome = teams >>= WS.wgtHome >>= WS.wgtTeamId
  in case (maway, mhome) of
       (Just a, Just h) ->
         ( []
         , Just Game
             { gameId       = GameId gid
             , gameDate     = day
             , gameAwayTeam = TeamId a
             , gameHomeTeam = TeamId h
             }
         )
       _ -> ([MissingTeamRef gid], Nothing)

parseDate :: Text -> Maybe Day
parseDate t = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (T.unpack t)

--------------------------------------------------------------------------------
-- Boxscore

-- | A boxscore yields *many* per-player entries; one per appearance per side.
-- We attach the team's MLB id so callers don't have to thread it back through
-- the structure.
data BoxscoreEntry = BoxscoreEntry
  { boxPlayerId :: PlayerId
  , boxTeamId   :: Maybe TeamId
  , boxBatting  :: BattingStats
  , boxPitching :: PitchingStats
  }
  deriving stock (Show, Eq)

-- | Convert a full boxscore. Currently emits no warnings — the wire format
-- is permissive enough that we can fill in missing pieces with empty stats.
-- We'll add tracing in Phase 3.
convertBoxscore :: WB.WireBoxscore -> ([ConvertWarning], [BoxscoreEntry])
convertBoxscore bs =
  let teams = WB.wbsTeams bs
      away  = boxsideEntries (WB.wbtAway teams)
      home  = boxsideEntries (WB.wbtHome teams)
  in ([], away <> home)

boxsideEntries :: WB.WireBoxTeam -> [BoxscoreEntry]
boxsideEntries side =
  map snd (Map.toList (Map.mapMaybeWithKey toEntry (WB.wbtPlayers side)))
  where
    toEntry _key wp = Just BoxscoreEntry
      { boxPlayerId = PlayerId (WB.wbpPersonId (WB.wbpPerson wp))
      , boxTeamId   = TeamId <$> WB.wbpParentTeamId wp
      , boxBatting  = maybe emptyBatting convertBatting
                        (WB.wbsBatting =<< WB.wbpStats wp)
      , boxPitching = maybe emptyPitching convertPitching
                        (WB.wbsPitching =<< WB.wbpStats wp)
      }

convertBatting :: WB.WireBoxBatting -> BattingStats
convertBatting WB.WireBoxBatting{..} = BattingStats
  { batGamesPlayed          = wbbGamesPlayed
  , batPlateAppearances     = wbbPlateAppearances
  , batAtBats               = wbbAtBats
  , batRuns                 = wbbRuns
  , batHits                 = wbbHits
  , batDoubles              = wbbDoubles
  , batTriples              = wbbTriples
  , batHomeRuns             = wbbHomeRuns
  , batRbi                  = wbbRbi
  , batBaseOnBalls          = wbbBaseOnBalls
  , batIntentionalWalks     = wbbIntentionalWalks
  , batStrikeOuts           = wbbStrikeOuts
  , batStolenBases          = wbbStolenBases
  , batCaughtStealing       = wbbCaughtStealing
  , batHitByPitch           = wbbHitByPitch
  , batSacBunts             = wbbSacBunts
  , batSacFlies             = wbbSacFlies
  , batGroundIntoDoublePlay = wbbGroundIntoDoublePlay
  , batGroundIntoTriplePlay = wbbGroundIntoTriplePlay
  , batLeftOnBase           = wbbLeftOnBase
  , batTotalBases           = wbbTotalBases
  , batFlyOuts              = wbbFlyOuts
  , batGroundOuts           = wbbGroundOuts
  , batCatchersInterference = wbbCatchersInterference
  , batPickoffs             = wbbPickoffs
  }

convertPitching :: WB.WireBoxPitching -> PitchingStats
convertPitching WB.WireBoxPitching{..} = PitchingStats
  { pitGamesPlayed             = wbpGamesPlayed
  , pitGamesStarted            = wbpGamesStarted
  , pitGamesFinished           = wbpGamesFinished
  , pitCompleteGames           = wbpCompleteGames
  , pitShutouts                = wbpShutouts
  , pitWins                    = wbpWins
  , pitLosses                  = wbpLosses
  , pitSaves                   = wbpSaves
  , pitSaveOpportunities       = wbpSaveOpportunities
  , pitHolds                   = wbpHolds
  , pitBlownSaves              = wbpBlownSaves
  , pitInningsPitched          = wbpInningsPitched
  , pitOuts                    = wbpOuts
  , pitBattersFaced            = wbpBattersFaced
  , pitNumberOfPitches         = wbpNumberOfPitches
  , pitStrikes                 = wbpStrikes
  , pitBalls                   = wbpBalls
  , pitHits                    = wbpHits
  , pitDoubles                 = wbpDoubles
  , pitTriples                 = wbpTriples
  , pitHomeRuns                = wbpHomeRuns
  , pitRuns                    = wbpRuns
  , pitEarnedRuns              = wbpEarnedRuns
  , pitStrikeOuts              = wbpStrikeOuts
  , pitBaseOnBalls             = wbpBaseOnBalls
  , pitIntentionalWalks        = wbpIntentionalWalks
  , pitHitBatsmen              = wbpHitBatsmen
  , pitWildPitches             = wbpWildPitches
  , pitBalks                   = wbpBalks
  , pitPickoffs                = wbpPickoffs
  , pitFlyOuts                 = wbpFlyOuts
  , pitGroundOuts              = wbpGroundOuts
  , pitAirOuts                 = wbpAirOuts
  , pitInheritedRunners        = wbpInheritedRunners
  , pitInheritedRunnersScored  = wbpInheritedRunnersScored
  , pitStolenBases             = wbpStolenBases
  , pitCaughtStealing          = wbpCaughtStealing
  , pitAtBats                  = wbpAtBats
  , pitRbi                     = wbpRbi
  , pitSacBunts                = wbpSacBunts
  , pitSacFlies                = wbpSacFlies
  , pitCatchersInterference    = wbpCatchersInterference
  , pitPassedBall              = wbpPassedBall
  }

--------------------------------------------------------------------------------
-- Internal helpers

orEmpty :: Maybe Text -> Text
orEmpty = maybe T.empty id

tshow :: Show a => a -> Text
tshow = T.pack . show