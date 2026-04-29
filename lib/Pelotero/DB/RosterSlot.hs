-- | Repository for the @roster_slot@ table.
--
-- A roster is the set of rows for a given @league_team_id@. There is no
-- roster-level identity; the roster *is* the set of slots. Operations
-- are all per-team: get all slots, add a player, remove a player,
-- replace the entire roster (for draft import), count per position.
module Pelotero.DB.RosterSlot
  ( -- * Row type
    RosterSlotRow(..)
    -- * Transaction-level API
  , getSlotsForTeamT
  , addSlotT
  , removeSlotT
  , removePlayerFromTeamT
  , clearTeamRosterT
  , replaceTeamRosterT
  , countBySlotT
    -- * Pool/IO API
  , getSlotsForTeam
  , addSlot
  , removeSlot
  , removePlayerFromTeam
  , clearTeamRoster
  , replaceTeamRoster
  , countBySlot
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int                   (Int64)
import Data.Text                  (Text)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Statement
import Pelotero.Domain.Id    (DbLeagueTeamId(..), DbPlayerId(..))

--------------------------------------------------------------------------------
-- Row type

data RosterSlotRow = RosterSlotRow
  { rsLeagueTeamId :: !DbLeagueTeamId
  , rsSlot         :: !Text
  , rsPlayerId     :: !DbPlayerId
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

getSlotsForTeamT :: DbLeagueTeamId -> Tx.Transaction [RosterSlotRow]
getSlotsForTeamT tid = V.toList <$> Tx.statement tid selectForTeamStmt

addSlotT :: RosterSlotRow -> Tx.Transaction ()
addSlotT row = Tx.statement row insertStmt

removeSlotT :: DbLeagueTeamId -> DbPlayerId -> Tx.Transaction ()
removeSlotT tid pid = Tx.statement (tid, pid) deleteOneStmt

removePlayerFromTeamT :: DbLeagueTeamId -> DbPlayerId -> Tx.Transaction ()
removePlayerFromTeamT = removeSlotT

clearTeamRosterT :: DbLeagueTeamId -> Tx.Transaction ()
clearTeamRosterT tid = Tx.statement tid deleteAllStmt

-- | Atomic roster replacement: clear then bulk insert. Used by the draft
-- to stamp the final roster in one transaction.
replaceTeamRosterT :: DbLeagueTeamId -> [RosterSlotRow] -> Tx.Transaction ()
replaceTeamRosterT tid rows = do
  clearTeamRosterT tid
  mapM_ addSlotT rows

countBySlotT :: DbLeagueTeamId -> Text -> Tx.Transaction Int64
countBySlotT tid slot = do
  mCount <- Tx.statement (tid, slot) countBySlotStmt
  pure (maybe 0 id mCount)

--------------------------------------------------------------------------------
-- Pool/IO API

getSlotsForTeam :: Pool -> DbLeagueTeamId -> IO (Either DBError [RosterSlotRow])
getSlotsForTeam pool tid = runTransaction pool (getSlotsForTeamT tid)

addSlot :: Pool -> RosterSlotRow -> IO (Either DBError ())
addSlot pool row = runTransaction pool (addSlotT row)

removeSlot :: Pool -> DbLeagueTeamId -> DbPlayerId -> IO (Either DBError ())
removeSlot pool tid pid = runTransaction pool (removeSlotT tid pid)

removePlayerFromTeam :: Pool -> DbLeagueTeamId -> DbPlayerId -> IO (Either DBError ())
removePlayerFromTeam = removeSlot

clearTeamRoster :: Pool -> DbLeagueTeamId -> IO (Either DBError ())
clearTeamRoster pool tid = runTransaction pool (clearTeamRosterT tid)

replaceTeamRoster :: Pool -> DbLeagueTeamId -> [RosterSlotRow] -> IO (Either DBError ())
replaceTeamRoster pool tid rows = runTransaction pool (replaceTeamRosterT tid rows)

countBySlot :: Pool -> DbLeagueTeamId -> Text -> IO (Either DBError Int64)
countBySlot pool tid slot = runTransaction pool (countBySlotT tid slot)

--------------------------------------------------------------------------------
-- Encoders / decoders

rowEncoder :: E.Params RosterSlotRow
rowEncoder =
     (rsLeagueTeamId >$< encDbLeagueTeamId)
  <> (rsSlot         >$< encText)
  <> (rsPlayerId     >$< encDbPlayerId)

rowDecoder :: D.Row RosterSlotRow
rowDecoder = RosterSlotRow
  <$> decDbLeagueTeamId
  <*> decText
  <*> decDbPlayerId

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement RosterSlotRow ()
insertStmt = Stmt.Statement sql rowEncoder D.noResult True
  where
    sql = "INSERT INTO roster_slot (league_team_id, slot, player_id) \
          \VALUES ($1, $2, $3) \
          \ON CONFLICT (league_team_id, player_id) DO UPDATE SET \
          \  slot = EXCLUDED.slot"

selectForTeamStmt :: Stmt.Statement DbLeagueTeamId (V.Vector RosterSlotRow)
selectForTeamStmt = Stmt.Statement sql encDbLeagueTeamId (D.rowVector rowDecoder) True
  where
    sql = "SELECT league_team_id, slot, player_id \
          \FROM roster_slot \
          \WHERE league_team_id = $1 \
          \ORDER BY slot, player_id"

deleteOneStmt :: Stmt.Statement (DbLeagueTeamId, DbPlayerId) ()
deleteOneStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "DELETE FROM roster_slot \
          \WHERE league_team_id = $1 AND player_id = $2"
    encoder = (fst >$< encDbLeagueTeamId) <> (snd >$< encDbPlayerId)

deleteAllStmt :: Stmt.Statement DbLeagueTeamId ()
deleteAllStmt = Stmt.Statement sql encDbLeagueTeamId D.noResult True
  where
    sql = "DELETE FROM roster_slot WHERE league_team_id = $1"

countBySlotStmt :: Stmt.Statement (DbLeagueTeamId, Text) (Maybe Int64)
countBySlotStmt = Stmt.Statement sql encoder (D.rowMaybe decInt64) True
  where
    sql = "SELECT COUNT(*) FROM roster_slot \
          \WHERE league_team_id = $1 AND slot = $2"
    encoder = (fst >$< encDbLeagueTeamId) <> (snd >$< encText)