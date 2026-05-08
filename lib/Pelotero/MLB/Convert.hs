-- | Wire-to-domain conversion for MLB API responses.
module Pelotero.MLB.Convert
  ( -- * Conversion
    convertPlayer
  , convertPlayers
  , convertSchedule
  , convertBoxscore
    -- * Reporting
  , ConvertWarning(..)
  , renderWarning
  -- , logWarnings
    -- * Box-score entries
  , BoxscoreEntry(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import           Control.Applicative          ((<|>))

import Pelotero.Domain.Game (Game(..), GameSchedule(..))
import Pelotero.Domain.Id (GameId(..), PlayerId(..), TeamId(..))
import Pelotero.Domain.Player
  ( Handedness
  , Player(..)
  , parseHandedness
  )
import Pelotero.Domain.Position (Position, parsePosition)
import Pelotero.Domain.Stats (BattingStats(..), PitchingStats(..), emptyPitching, parseInningsPitched)
import qualified Pelotero.MLB.Wire.Boxscore as WB
import qualified Pelotero.MLB.Wire.Player as WP
import qualified Pelotero.MLB.Wire.Schedule as WS

--------------------------------------------------------------------------------
-- Warnings

data ConvertWarning
  = InvalidPlayerId !Int
  | UnknownPosition !Int !Text
  | UnknownHandedness !Int !Text
  | InvalidGameDate !Int !Text
  | MissingTeamRef !Int
  | WireFieldDiscrepancy !Int !Text !Int   -- ^ player id, wire IP text, wire outs
  deriving stock (Show, Eq)

renderWarning :: ConvertWarning -> Text
renderWarning = \case
  InvalidPlayerId pid ->
    "convert: dropped player with invalid id " <> tshow pid
  UnknownPosition pid code ->
    "convert: player " <> tshow pid
      <> " has unknown position code " <> tshow code
      <> "; setting Nothing"
  UnknownHandedness pid code ->
    "convert: player " <> tshow pid
      <> " has unknown hand code " <> tshow code
      <> "; setting Nothing"
  InvalidGameDate gid raw ->
    "convert: dropped game " <> tshow gid
      <> " with unparseable date " <> tshow raw
  MissingTeamRef gid ->
    "convert: dropped game " <> tshow gid <> " missing team reference"
  WireFieldDiscrepancy pid ip outs ->
    "convert: pitching " <> tshow pid
      <> " wire field discrepancy: ip=" <> ip
      <> " outs=" <> tshow outs

-- logWarnings :: [ConvertWarning] -> IO ()
-- logWarnings = logWarningsTo stderr

-- logWarningsTo :: Handle -> [ConvertWarning] -> IO ()
-- logWarningsTo h ws = unless (null ws) $
--   mapM_ (\w -> hPutStrLn h (T.unpack (renderWarning w))) ws

--------------------------------------------------------------------------------
-- Players

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
    Nothing  -> ([], [])
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

-- | One entry per player appearance per side. The 'GameId' is supplied by
-- the caller; the wire format doesn't carry it because by the time you have
-- a boxscore in hand you also have the game ID from the URL it was fetched
-- at.
--
-- 'boxBatting' and 'boxPitching' are 'Maybe' to preserve the wire-level
-- distinction between "this player batted" and "this player did not bat" /
-- "this player pitched" and "this player did not pitch". A position player
-- in an AL game has 'Nothing' for pitching; a relief pitcher who did not
-- come to the plate has 'Nothing' for batting. The DB-write layer skips
-- 'Nothing' rather than inserting an all-null row.
data BoxscoreEntry = BoxscoreEntry
  { boxGameId   :: !GameId
  , boxPlayerId :: !PlayerId
  , boxTeamId   :: !(Maybe TeamId)
  , boxBatting  :: !(Maybe BattingStats)
  , boxPitching :: !(Maybe PitchingStats)
  }
  deriving stock (Show, Eq)

convertBoxscore :: GameId -> WB.WireBoxscore -> ([ConvertWarning], [BoxscoreEntry])
convertBoxscore gid bs =
  let (aw, ae) = boxsideEntries gid (WB.wbtAway (WB.wbsTeams bs))
      (hw, he) = boxsideEntries gid (WB.wbtHome (WB.wbsTeams bs))
  in (aw <> hw, ae <> he)

boxsideEntries :: GameId -> WB.WireBoxTeam -> ([ConvertWarning], [BoxscoreEntry])
boxsideEntries gid team =
  let pairs = map mkOne (Map.elems (WB.wbtPlayers team))
      (warnsList, entries) = unzip pairs
  in (concat warnsList, entries)
  where
    mkOne wp =
      let pid = WB.wbpPersonId (WB.wbpPerson wp)
          stats = WB.wbpStats wp
          batting = fmap convertBatting (stats >>= WB.wbsBatting)
          (pitWarns, mPitching) = case stats >>= WB.wbsPitching of
            Just wbpitch ->
              let (ws, ps) = convertPitching pid wbpitch
              in (ws, Just ps)
            Nothing -> ([], Nothing)
      in (pitWarns, BoxscoreEntry
            { boxGameId   = gid
            , boxPlayerId = PlayerId pid
            , boxTeamId   = fmap TeamId (WB.wbpParentTeamId wp)
            , boxBatting  = batting
            , boxPitching = mPitching
            })

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

convertPitching :: Int -> WB.WireBoxPitching -> ([ConvertWarning], PitchingStats)
convertPitching playerId wp =
  let parsedIp = WB.wbpInningsPitched wp >>= parseInningsPitched
      wireOuts = WB.wbpOuts wp
      warns    = case (WB.wbpInningsPitched wp, parsedIp, wireOuts) of
        (Just ipText, Just ipOuts, Just wOuts) | ipOuts /= wOuts ->
          [WireFieldDiscrepancy playerId ipText wOuts]
        _ -> []
      stats = emptyPitching
        { pitGamesPlayed             = WB.wbpGamesPlayed wp
        , pitGamesStarted            = WB.wbpGamesStarted wp
        , pitGamesFinished           = WB.wbpGamesFinished wp
        , pitCompleteGames           = WB.wbpCompleteGames wp
        , pitShutouts                = WB.wbpShutouts wp
        , pitWins                    = WB.wbpWins wp
        , pitLosses                  = WB.wbpLosses wp
        , pitSaves                   = WB.wbpSaves wp
        , pitSaveOpportunities       = WB.wbpSaveOpportunities wp
        , pitHolds                   = WB.wbpHolds wp
        , pitBlownSaves              = WB.wbpBlownSaves wp
        , pitOuts                    = parsedIp <|> wireOuts
        , pitBattersFaced            = WB.wbpBattersFaced wp
        , pitNumberOfPitches         = WB.wbpNumberOfPitches wp
        , pitStrikes                 = WB.wbpStrikes wp
        , pitBalls                   = WB.wbpBalls wp
        , pitHits                    = WB.wbpHits wp
        , pitDoubles                 = WB.wbpDoubles wp
        , pitTriples                 = WB.wbpTriples wp
        , pitHomeRuns                = WB.wbpHomeRuns wp
        , pitRuns                    = WB.wbpRuns wp
        , pitEarnedRuns              = WB.wbpEarnedRuns wp
        , pitStrikeOuts              = WB.wbpStrikeOuts wp
        , pitBaseOnBalls             = WB.wbpBaseOnBalls wp
        , pitIntentionalWalks        = WB.wbpIntentionalWalks wp
        , pitHitBatsmen              = WB.wbpHitBatsmen wp
        , pitWildPitches             = WB.wbpWildPitches wp
        , pitBalks                   = WB.wbpBalks wp
        , pitPickoffs                = WB.wbpPickoffs wp
        , pitFlyOuts                 = WB.wbpFlyOuts wp
        , pitGroundOuts              = WB.wbpGroundOuts wp
        , pitAirOuts                 = WB.wbpAirOuts wp
        , pitInheritedRunners        = WB.wbpInheritedRunners wp
        , pitInheritedRunnersScored  = WB.wbpInheritedRunnersScored wp
        , pitStolenBases             = WB.wbpStolenBases wp
        , pitCaughtStealing          = WB.wbpCaughtStealing wp
        , pitAtBats                  = WB.wbpAtBats wp
        , pitRbi                     = WB.wbpRbi wp
        , pitSacBunts                = WB.wbpSacBunts wp
        , pitSacFlies                = WB.wbpSacFlies wp
        , pitCatchersInterference    = WB.wbpCatchersInterference wp
        , pitPassedBall              = WB.wbpPassedBall wp
        }
  in (warns, stats)

--------------------------------------------------------------------------------
-- Internal helpers

orEmpty :: Maybe Text -> Text
orEmpty = maybe T.empty id

tshow :: Show a => a -> Text
tshow = T.pack . show