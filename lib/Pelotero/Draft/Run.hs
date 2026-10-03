{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | Effectful handler that drives the 'Pelotero.Draft.Machine' state
-- machine and persists its events. Three layers:
--
-- * 'applyAndPersist' is the per-command primitive, used both by
--   'runAutoDraft' and (eventually) by a manual-pick UI.
--
-- * 'runAutoDraft' is the auto-pick loop. It drives the state machine
--   from start to finish, picking for whichever team is up by reading
--   their 'PlayerRanking'. Takes a pre-built 'DraftPlan'.
--
-- * 'runAutoDraftForLeague' is the operator-facing entry point used
--   by the CLI. Builds the 'DraftPlan' from a league config id
--   (resolving teams, players, strategy, and pick count) and delegates
--   to 'runAutoDraft'.
--
-- Every accepted pick writes two rows in one database transaction:
-- the @draft_pick@ row and the @roster_slot@ row for the slot the
-- state machine placed the player in. When the draft completes, each
-- team's @roster_slot@ rows are exactly its drafted roster, and if the
-- process dies mid-draft the two tables still agree with each other.
--
-- League lifecycle (moving @league_config.status@ from @"draft"@ to
-- @"active"@, setting lineups) is not done here. See
-- "Pelotero.League.Setup".
module Pelotero.Draft.Run
  ( -- * Errors
    AutoDraftError (..)
    -- * Per-command persistence primitive
  , applyAndPersist
    -- * Auto-draft loop
  , runAutoDraft
    -- * Operator-facing entry point
  , runAutoDraftForLeague
    -- * Pool construction
  , draftablePool
  ) where

import           Data.Foldable             (find, traverse_)
import           Data.Int                  (Int64)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (isJust)
import qualified Data.Text                 as T
import           Effectful
import qualified Katip                     as K

import           Pelotero.DB.DraftPick     (DraftPickRow (..))
import           Pelotero.DB.LeagueConfig  (LoadedLeagueConfig (..))
import           Pelotero.DB.LeagueTeam    (LoadedLeagueTeam (..))
import           Pelotero.DB.Player        (LoadedPlayerRow (..))
import           Pelotero.DB.PlayerRanking (PlayerRankingRow (..))
import           Pelotero.DB.RosterSlot    (RosterSlotRow (..))
import           Pelotero.Domain.Draft
                     ( generateDraftOrder
                     , parseDraftOrderStrategy
                     )
import           Pelotero.Domain.Id
                     ( DbLeagueConfigId
                     , DbLeagueTeamId
                     , DbPlayerId
                     , DraftPickNumber (..)
                     )
import           Pelotero.Domain.League
                     ( LeagueStatus (..)
                     , parseLeagueStatus
                     )
import           Pelotero.Domain.Position  (Position, parsePosition)
import           Pelotero.Domain.Roster    (renderRosterSlot, totalRosterSize)
import           Pelotero.Draft
                     ( DraftCommand (..)
                     , DraftContext (..)
                     , DraftError
                     , DraftEvent (..)
                     , DraftPickEntry (..)
                     , DraftPlan (..)
                     , DraftSummary (..)
                     , acceptedSlot
                     )
import           Pelotero.Draft.Machine
                     ( DraftStateG (..)
                     , DraftVertex (..)
                     , SomeDraftStateG (..)
                     , initialMachineState
                     , runDraftCommand
                     )
import           Pelotero.Effects.Clock         (Clock)
import qualified Pelotero.Effects.Clock         as Clock
import           Pelotero.Effects.DraftPick     (DraftPick)
import qualified Pelotero.Effects.DraftPick     as DP
import           Pelotero.Effects.LeagueConfig  (LeagueConfig)
import qualified Pelotero.Effects.LeagueConfig  as LC
import           Pelotero.Effects.LeagueTeam    (LeagueTeam)
import qualified Pelotero.Effects.LeagueTeam    as LT
import           Pelotero.Effects.Logging       (Logging)
import qualified Pelotero.Effects.Logging       as Logging
import           Pelotero.Effects.PlayerRanking (PlayerRanking)
import qualified Pelotero.Effects.PlayerRanking as PR
import           Pelotero.Effects.Players       (Players)
import qualified Pelotero.Effects.Players       as Players

-- ---------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------

-- | Errors specific to the auto-draft loop. Distinct from
-- 'DraftError' (the pure state-machine rejections) because the loop
-- has its own ways to fail beyond what the state machine cares about.
data AutoDraftError
  = AutoDraftRejected      !DraftError
    -- ^ State machine rejected a command issued by the loop itself.
    --   For 'StartDraft' this reports an unusable plan, including one
    --   the pool cannot complete ('PlanInfeasible'), and nothing has
    --   been written. For 'MakePick' it indicates a bug in this module,
    --   because the loop only issues picks that 'acceptedSlot' accepts.
  | AutoDraftNoCandidate   !DbLeagueTeamId
    -- ^ No available player is an acceptable pick for this team. Should
    --   be unreachable: the machine starts only plans the pool can
    --   complete and refuses any pick that would make the rest
    --   impossible, so the team on the clock always has an acceptable
    --   pick. Included as a defensive catch, like 'AutoDraftStuck'.
    --   Picks made before this point remain persisted.
  | AutoDraftStuck         !DraftVertex
    -- ^ Loop saw the state machine in an unexpected vertex
    --   (e.g. 'WaitingToStartV' after StartDraft was supposed to
    --   succeed). Should be unreachable; included as a defensive
    --   catch.
  | AutoDraftConfigMissing !DbLeagueConfigId
    -- ^ 'runAutoDraftForLeague' was called for a league id that
    --   doesn't exist in 'league_config'.
  | AutoDraftNotInDraft    !DbLeagueConfigId !T.Text
    -- ^ The league's status is not @"draft"@. Carries the status found.
  | AutoDraftAlreadyHasPicks !DbLeagueConfigId !Int64
    -- ^ The league already has persisted picks. Re-running would hit
    --   the unique constraints on @draft_pick@, so it is refused up
    --   front.
  | AutoDraftNoTeams       !DbLeagueConfigId
    -- ^ 'runAutoDraftForLeague' found the league config but no rows
    --   in 'league_team' for it.
  | AutoDraftBadStrategy   !T.Text
    -- ^ 'llcDraftStrategy' didn't parse via 'parseDraftOrderStrategy'.
    --   Carries the offending Text so operators can grep for it.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------
-- Per-command persistence primitive
-- ---------------------------------------------------------------------

-- | Apply a single command and persist whichever events come out.
-- Rejections are logged at WarningS and the state is unchanged
-- (relies on 'DraftTopology'\'s self-loops).
--
-- The caller threads 'SomeDraftStateG' across calls explicitly; there
-- is no effect for state holding. Keeps the primitive testable with the
-- existing in-memory effect interpreters.
applyAndPersist
  :: ( DraftPick :> es
     , Clock     :> es
     , Logging   :> es
     )
  => DbLeagueConfigId
  -> SomeDraftStateG
  -> DraftCommand
  -> Eff es (Either DraftError [DraftEvent], SomeDraftStateG)
applyAndPersist league someSt cmd = do
  let (out, someSt') = runDraftCommand someSt cmd
  case out of
    Left err -> do
      Logging.logFM K.WarningS $
        "Draft command rejected: " <> tshow err
      pure (Left err, someSt')
    Right events -> do
      traverse_ (persistEvent league) events
      pure (Right events, someSt')

-- | Persist one event. 'PickRecorded' becomes one
-- 'DP.recordPickWithSlot', which writes the pick and its roster slot
-- atomically; 'DraftStarted' and 'DraftCompleted' are log-only.
persistEvent
  :: ( DraftPick :> es
     , Clock     :> es
     , Logging   :> es
     )
  => DbLeagueConfigId
  -> DraftEvent
  -> Eff es ()
persistEvent league = \case
  DraftStarted leagueId teams ->
    Logging.logFM K.InfoS $
      "Draft started for league " <> tshow leagueId
        <> " with " <> tshow (length teams) <> " teams"

  PickRecorded DraftPickEntry{..} -> do
    now <- Clock.now
    let row = DraftPickRow
          { dpId             = Nothing
          , dpLeagueConfigId = league
          , dpPickNumber     = fromIntegral (unDraftPickNumber dpePickNumber)
          , dpLeagueTeamId   = dpeTeam
          , dpPlayerId       = dpePlayer
          , dpPickedAt       = Just now
          }
        slot = RosterSlotRow
          { rsLeagueTeamId = dpeTeam
          , rsSlot         = renderRosterSlot dpeSlot
          , rsPlayerId     = dpePlayer
          }
    _ <- DP.recordPickWithSlot row slot
    Logging.logFM K.DebugS $
      "Recorded pick #" <> tshow (unDraftPickNumber dpePickNumber)
        <> " for team " <> tshow dpeTeam
        <> " at " <> renderRosterSlot dpeSlot

  DraftCompleted summary ->
    Logging.logFM K.InfoS $
      "Draft completed for league " <> tshow (dsLeague summary)
        <> " with " <> tshow (length (dsPicks summary)) <> " picks"

-- ---------------------------------------------------------------------
-- Auto-draft loop
-- ---------------------------------------------------------------------

-- | Drive a draft from 'initialMachineState' to 'CompleteG' via
-- auto-picks. Persists each pick as it happens. Returns the
-- 'DraftSummary' on completion or an 'AutoDraftError' on failure.
--
-- The 'forall es.' is load-bearing: it brings @es@ into scope for the
-- 'driveLoop' helper's own type signature in the @where@ clause, so
-- the effect constraints carry through.
--
-- Single-process, in-memory state. If the loop dies mid-draft the
-- already-inserted rows must be cleaned up before retry;
-- 'runAutoDraftForLeague' refuses to start on a league that already
-- has picks.
runAutoDraft
  :: forall es.
     ( DraftPick     :> es
     , PlayerRanking :> es
     , Clock         :> es
     , Logging       :> es
     )
  => DraftPlan
  -> Eff es (Either AutoDraftError DraftSummary)
runAutoDraft plan = do
  Logging.logFM K.InfoS $
    "Starting auto-draft for league " <> tshow (dpLeague plan)
      <> " with " <> tshow (length (dpOrder plan)) <> " picks"
      <> " from a pool of " <> tshow (Map.size (dpPool plan))
  (out, st1) <- applyAndPersist
                  (dpLeague plan)
                  initialMachineState
                  (StartDraft plan)
  case out of
    Left err -> pure (Left (AutoDraftRejected err))
    Right _  -> driveLoop st1
  where
    driveLoop
      :: SomeDraftStateG
      -> Eff es (Either AutoDraftError DraftSummary)
    driveLoop someSt@(SomeDraftStateG _ st) = case st of
      WaitingToStartG ->
        pure (Left (AutoDraftStuck WaitingToStartV))
      CompleteG summary ->
        pure (Right summary)
      DraftingG ctx -> do
        autoCmd <- autoPickCommand ctx
        case autoCmd of
          Left err  -> pure (Left err)
          Right cmd -> do
            (out', st') <- applyAndPersist (dpLeague plan) someSt cmd
            case out' of
              Left rejErr -> pure (Left (AutoDraftRejected rejErr))
              Right _     -> driveLoop st'

-- | Pick a player for the team whose turn it currently is. Reads the
-- team's 'PlayerRanking' and takes the highest-ranked player who is
-- an acceptable pick: still available, fits an open slot on the team's
-- roster, and leaves the rest of the draft completable ('acceptedSlot').
-- A ranked player who fits but would leave another team unable to fill
-- its roster is skipped. If no ranked player qualifies, falls back to the
-- lowest-id acceptable player, with a 'WarningS' log line.
autoPickCommand
  :: ( PlayerRanking :> es
     , Logging       :> es
     )
  => DraftContext
  -> Eff es (Either AutoDraftError DraftCommand)
autoPickCommand ctx = case dcRemaining ctx of
  [] ->
    -- Defensive: a Drafting state with empty remaining shouldn't
    -- reach this branch in normal flow.
    pure (Left (AutoDraftStuck DraftingV))
  (team, _) : _ -> do
    rankings <- PR.getRankingsForTeam team
    let fits pid = isJust (acceptedSlot ctx team pid)
    case find fits (map prPlayerId rankings) of
      Just pid ->
        pure (Right (MakePick team pid))
      Nothing -> case find fits (Map.keys (dcPool ctx)) of
        Just pid -> do
          Logging.logFM K.WarningS $
            "No ranked candidate fits team " <> tshow team
              <> "; falling back to lowest-id acceptable player"
          pure (Right (MakePick team pid))
        Nothing ->
          pure (Left (AutoDraftNoCandidate team))

-- ---------------------------------------------------------------------
-- Operator-facing entry point
-- ---------------------------------------------------------------------

-- | The draftable pool: active players whose stored position parses.
-- A player with no position, or with one the engine does not model
-- (two-way players), cannot be placed in a roster slot and is left out.
draftablePool :: [LoadedPlayerRow] -> Map DbPlayerId Position
draftablePool players = Map.fromList
  [ (lprId p, position)
  | p <- players
  , lprActive p
  , Just position <- [lprPosition p >>= parsePosition]
  ]

-- | Build a 'DraftPlan' from a league config id and run the
-- auto-draft loop. Fails fast, before any write, when:
--
-- * the league does not exist ('AutoDraftConfigMissing');
-- * its status is not @"draft"@ ('AutoDraftNotInDraft');
-- * it already has persisted picks ('AutoDraftAlreadyHasPicks');
-- * its strategy does not parse ('AutoDraftBadStrategy');
-- * it has no teams ('AutoDraftNoTeams').
--
-- Picks per team is 'totalRosterSize' over the league's
-- 'RosterLimits'; total pick count is @picksPerTeam * length teams@.
runAutoDraftForLeague
  :: ( DraftPick     :> es
     , PlayerRanking :> es
     , Clock         :> es
     , Logging       :> es
     , LeagueConfig  :> es
     , LeagueTeam    :> es
     , Players       :> es
     )
  => DbLeagueConfigId
  -> Eff es (Either AutoDraftError DraftSummary)
runAutoDraftForLeague lcid = do
  mConfig <- LC.getById lcid
  case mConfig of
    Nothing ->
      refuse (AutoDraftConfigMissing lcid)
    Just config
      | parseLeagueStatus (llcStatus config) /= Just LeagueDraft ->
          refuse (AutoDraftNotInDraft lcid (llcStatus config))
      | otherwise ->
          case parseDraftOrderStrategy (llcDraftStrategy config) of
            Nothing ->
              refuse (AutoDraftBadStrategy (llcDraftStrategy config))
            Just strategy -> do
              existing <- DP.getPickCount lcid
              teams    <- LT.getForLeague lcid
              case teams of
                _ | existing > 0 ->
                      refuse (AutoDraftAlreadyHasPicks lcid existing)
                [] ->
                  refuse (AutoDraftNoTeams lcid)
                _ -> do
                  activePlayers <- Players.getActivePlayers
                  let teamIds      = map lltId teams
                      limits       = llcRosterLimits config
                      picksPerTeam = totalRosterSize limits
                      totalPicks   = picksPerTeam * length teamIds
                      plan         = DraftPlan
                        { dpLeague = lcid
                        , dpOrder  = generateDraftOrder strategy totalPicks teamIds
                        , dpPool   = draftablePool activePlayers
                        , dpLimits = limits
                        }
                  runAutoDraft plan
  where
    refuse err = do
      Logging.logFM K.ErrorS $ "Auto-draft refused: " <> tshow err
      pure (Left err)

-- ---------------------------------------------------------------------
-- Internal helper
-- ---------------------------------------------------------------------

tshow :: Show a => a -> T.Text
tshow = T.pack . show