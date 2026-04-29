-- | Repository for the @lineup_slot@ table. Same shape as 'RosterSlot';
-- the distinction is semantic (active lineup vs. full roster).
module Pelotero.DB.LineupSlot
  ( LineupSlotRow(..)
  , getSlotsForTeamT
  , addSlotT
  , removeSlotT
  , clearTeamLineupT
  , replaceTeamLineupT
  , getSlotsForTeam
  , addSlot
  , removeSlot
  , clearTeamLineup
  , replaceTeamLineup
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text                  (Text)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Statement
import Pelotero.Domain.Id    (DbLeagueTeamId(..), DbPlayerId(..))

data LineupSlotRow = LineupSlotRow
  { lsLeagueTeamId :: !DbLeagueTeamId
  , lsSlot         :: !Text
  , lsPlayerId     :: !DbPlayerId
  }
  deriving stock (Show, Eq)

getSlotsForTeamT :: DbLeagueTeamId -> Tx.Transaction [LineupSlotRow]
getSlotsForTeamT tid = V.toList <$> Tx.statement tid selectForTeamStmt

addSlotT :: LineupSlotRow -> Tx.Transaction ()
addSlotT row = Tx.statement row insertStmt

removeSlotT :: DbLeagueTeamId -> DbPlayerId -> Tx.Transaction ()
removeSlotT tid pid = Tx.statement (tid, pid) deleteOneStmt

clearTeamLineupT :: DbLeagueTeamId -> Tx.Transaction ()
clearTeamLineupT tid = Tx.statement tid deleteAllStmt

replaceTeamLineupT :: DbLeagueTeamId -> [LineupSlotRow] -> Tx.Transaction ()
replaceTeamLineupT tid rows = do
  clearTeamLineupT tid
  mapM_ addSlotT rows

getSlotsForTeam :: Pool -> DbLeagueTeamId -> IO (Either DBError [LineupSlotRow])
getSlotsForTeam pool tid = runTransaction pool (getSlotsForTeamT tid)

addSlot :: Pool -> LineupSlotRow -> IO (Either DBError ())
addSlot pool row = runTransaction pool (addSlotT row)

removeSlot :: Pool -> DbLeagueTeamId -> DbPlayerId -> IO (Either DBError ())
removeSlot pool tid pid = runTransaction pool (removeSlotT tid pid)

clearTeamLineup :: Pool -> DbLeagueTeamId -> IO (Either DBError ())
clearTeamLineup pool tid = runTransaction pool (clearTeamLineupT tid)

replaceTeamLineup :: Pool -> DbLeagueTeamId -> [LineupSlotRow] -> IO (Either DBError ())
replaceTeamLineup pool tid rows = runTransaction pool (replaceTeamLineupT tid rows)

rowEncoder :: E.Params LineupSlotRow
rowEncoder =
     (lsLeagueTeamId >$< encDbLeagueTeamId)
  <> (lsSlot         >$< encText)
  <> (lsPlayerId     >$< encDbPlayerId)

rowDecoder :: D.Row LineupSlotRow
rowDecoder = LineupSlotRow
  <$> decDbLeagueTeamId
  <*> decText
  <*> decDbPlayerId

insertStmt :: Stmt.Statement LineupSlotRow ()
insertStmt = Stmt.Statement sql rowEncoder D.noResult True
  where
    sql = "INSERT INTO lineup_slot (league_team_id, slot, player_id) \
          \VALUES ($1, $2, $3) \
          \ON CONFLICT (league_team_id, player_id) DO UPDATE SET \
          \  slot = EXCLUDED.slot"

selectForTeamStmt :: Stmt.Statement DbLeagueTeamId (V.Vector LineupSlotRow)
selectForTeamStmt = Stmt.Statement sql encDbLeagueTeamId (D.rowVector rowDecoder) True
  where
    sql = "SELECT league_team_id, slot, player_id \
          \FROM lineup_slot \
          \WHERE league_team_id = $1 \
          \ORDER BY slot, player_id"

deleteOneStmt :: Stmt.Statement (DbLeagueTeamId, DbPlayerId) ()
deleteOneStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "DELETE FROM lineup_slot \
          \WHERE league_team_id = $1 AND player_id = $2"
    encoder = (fst >$< encDbLeagueTeamId) <> (snd >$< encDbPlayerId)

deleteAllStmt :: Stmt.Statement DbLeagueTeamId ()
deleteAllStmt = Stmt.Statement sql encDbLeagueTeamId D.noResult True
  where
    sql = "DELETE FROM lineup_slot WHERE league_team_id = $1"