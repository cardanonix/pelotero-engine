{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Pelotero.App
-- Description : Standard executable runner stack.
--
-- 'runApp' is the canonical production runner. There is exactly one
-- defensible discharge order given the dependencies between
-- interpreters; this module pins it.
--
-- Constraints that fix the order:
--
--  * 'runDatabasePool' needs 'Logging', 'IOE', and 'Error' 'DBError'
--    available in scope (it logs on the way to 'throwError'), so
--    'Database' must be discharged BEFORE 'Logging' / 'Error'.
--
--  * Every repository runner ('runPlayersDB', 'runTeamsDB', ...)
--    needs 'Database' available, so they must be discharged before
--    'Database'.
--
--  * 'runClockIO' and 'runMLBClientHTTP' only need 'IOE'; they sit
--    between 'Database' and 'Logging' for visual grouping but are
--    structurally independent of either.
--
-- Actions written with constraint-polymorphic types
-- (@'(Players' ':>' es, 'FetchLog' ':>' es) => 'Eff' es ()@) work
-- with 'runApp' without explicit lifting. Closed-stack actions
-- (@'Eff' '\'[Players, FetchLog]' ()@) need 'Effectful.inject'.
--
-- For tests, assemble a stack from the in-memory interpreters
-- directly. There is no one-size-fits-all test runner because test
-- scenarios differ in which interpreters they need, and pasting a
-- stack at the test site is cheaper than maintaining N flavours of
-- test runner here.
module Pelotero.App
  ( AppEffects
  , runApp
  ) where

import           Effectful
import           Effectful.Error.Static          (Error, runErrorNoCallStack)
import           Pelotero.DB.Pool                (DBError, Pool)
import qualified Pelotero.Effects.BoxscoreEntry  as Box
import qualified Pelotero.Effects.Clock          as Clock
import qualified Pelotero.Effects.Database       as DB
import qualified Pelotero.Effects.DraftPick      as DraftPick
import qualified Pelotero.Effects.FetchLog       as FetchLog
import qualified Pelotero.Effects.Games          as Games
import qualified Pelotero.Effects.LeagueConfig   as LC
import qualified Pelotero.Effects.LeagueTeam     as LT
import qualified Pelotero.Effects.LineupSlot     as LS
import           Pelotero.Effects.Logging        (LogEnv)
import qualified Pelotero.Effects.Logging        as Logging
import qualified Pelotero.Effects.MLBClient      as MLB
import qualified Pelotero.Effects.PlayerRanking  as PR
import qualified Pelotero.Effects.Players        as Players
import qualified Pelotero.Effects.RosterSlot     as RS
import qualified Pelotero.Effects.Teams          as Teams
import qualified Pelotero.Effects.LineupSnapshot as LSnap

-- | The full effect list available to actions inside 'runApp', in
-- discharge order (head = innermost = first to be removed).
type AppEffects =
  '[ Players.Players
   , Teams.Teams
   , Games.Games
   , Box.BoxscoreEntry
   , FetchLog.FetchLog
   , LS.LineupSlot
   , LSnap.LineupSnapshot
   , RS.RosterSlot
   , LC.LeagueConfig
   , LT.LeagueTeam
   , PR.PlayerRanking
   , DraftPick.DraftPick
   , DB.Database
   , Clock.Clock
   , MLB.MLBClient
   , Logging.Logging
   , Error DBError
   , IOE
   ]
-- | Discharge an 'AppEffects' action against a live database pool
-- and a configured Katip 'LogEnv'. Returns 'Right' on success or
-- 'Left' on the first uncaught 'DBError'. The error is also logged
-- at 'ErrorS' before being raised.
--
-- Uses 'runErrorNoCallStack' rather than 'runError' because the
-- 'DBError' is already logged with full rendered context inside
-- 'runDatabasePool' before being thrown; the 'CallStack' at this
-- outermost handler doesn't add value over what the log already has.
runApp :: Pool -> LogEnv -> Eff AppEffects a -> IO (Either DBError a)
runApp pool logEnv =
    runEff
  . runErrorNoCallStack @DBError
  . Logging.runLoggingKatip logEnv
  . MLB.runMLBClientHTTP
  . Clock.runClockIO
  . DB.runDatabasePool pool
  . DraftPick.runDraftPickDB
  . PR.runPlayerRankingDB
  . LT.runLeagueTeamDB
  . LC.runLeagueConfigDB
  . RS.runRosterSlotDB
  . LSnap.runLineupSnapshotDB
  . LS.runLineupSlotDB
  . FetchLog.runFetchLogDB
  . Box.runBoxscoreEntryDB
  . Games.runGamesDB
  . Teams.runTeamsDB
  . Players.runPlayersDB