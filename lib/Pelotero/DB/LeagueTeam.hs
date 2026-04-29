-- | Repository for the @league_team@ table. A league_team is a fantasy
-- team within a league, identified by @(league_config_id, team_key)@.
module Pelotero.DB.LeagueTeam
  ( -- * Row type
    LeagueTeamRow(..)
    -- * Transaction-level API
  , insertLeagueTeamT
  , updateLeagueTeamT
  , getByIdT
  , lookupByKeyT
  , getForLeagueT
  , deleteT
    -- * Pool/IO API
  , insertLeagueTeam
  , updateLeagueTeam
  , getById
  , lookupByKey
  , getForLeague
  , delete
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
import Pelotero.Domain.Id    (DbLeagueConfigId(..), DbLeagueTeamId(..))

--------------------------------------------------------------------------------
-- Row type

data LeagueTeamRow = LeagueTeamRow
  { ltId             :: !(Maybe DbLeagueTeamId)
  , ltLeagueConfigId :: !DbLeagueConfigId
  , ltTeamKey        :: !Text
  , ltName           :: !Text
  , ltOwner          :: !Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

insertLeagueTeamT :: LeagueTeamRow -> Tx.Transaction DbLeagueTeamId
insertLeagueTeamT row = Tx.statement row insertStmt

updateLeagueTeamT :: DbLeagueTeamId -> LeagueTeamRow -> Tx.Transaction ()
updateLeagueTeamT ltid row = Tx.statement (ltid, row) updateStmt

getByIdT :: DbLeagueTeamId -> Tx.Transaction (Maybe LeagueTeamRow)
getByIdT ltid = Tx.statement ltid selectByIdStmt

lookupByKeyT
  :: DbLeagueConfigId -> Text -> Tx.Transaction (Maybe LeagueTeamRow)
lookupByKeyT lcid key = Tx.statement (lcid, key) selectByKeyStmt

getForLeagueT :: DbLeagueConfigId -> Tx.Transaction [LeagueTeamRow]
getForLeagueT lcid = V.toList <$> Tx.statement lcid selectForLeagueStmt

deleteT :: DbLeagueTeamId -> Tx.Transaction ()
deleteT ltid = Tx.statement ltid deleteStmt

--------------------------------------------------------------------------------
-- Pool/IO API

insertLeagueTeam :: Pool -> LeagueTeamRow -> IO (Either DBError DbLeagueTeamId)
insertLeagueTeam pool row = runTransaction pool (insertLeagueTeamT row)

updateLeagueTeam :: Pool -> DbLeagueTeamId -> LeagueTeamRow -> IO (Either DBError ())
updateLeagueTeam pool ltid row = runTransaction pool (updateLeagueTeamT ltid row)

getById :: Pool -> DbLeagueTeamId -> IO (Either DBError (Maybe LeagueTeamRow))
getById pool ltid = runTransaction pool (getByIdT ltid)

lookupByKey
  :: Pool -> DbLeagueConfigId -> Text -> IO (Either DBError (Maybe LeagueTeamRow))
lookupByKey pool lcid key = runTransaction pool (lookupByKeyT lcid key)

getForLeague :: Pool -> DbLeagueConfigId -> IO (Either DBError [LeagueTeamRow])
getForLeague pool lcid = runTransaction pool (getForLeagueT lcid)

delete :: Pool -> DbLeagueTeamId -> IO (Either DBError ())
delete pool ltid = runTransaction pool (deleteT ltid)

--------------------------------------------------------------------------------
-- Encoder

insertEncoder :: E.Params LeagueTeamRow
insertEncoder =
     (ltLeagueConfigId >$< encDbLeagueConfigId)
  <> (ltTeamKey        >$< encText)
  <> (ltName           >$< encText)
  <> (ltOwner          >$< encText)

--------------------------------------------------------------------------------
-- Decoder

rowDecoder :: D.Row LeagueTeamRow
rowDecoder = LeagueTeamRow
  <$> (Just <$> decDbLeagueTeamId)
  <*> decDbLeagueConfigId
  <*> decText
  <*> decText
  <*> decText

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement LeagueTeamRow DbLeagueTeamId
insertStmt = Stmt.Statement sql insertEncoder (D.singleRow decDbLeagueTeamId) True
  where
    sql = "INSERT INTO league_team \
          \  (league_config_id, team_key, name, owner) \
          \VALUES ($1, $2, $3, $4) \
          \RETURNING id"

updateStmt :: Stmt.Statement (DbLeagueTeamId, LeagueTeamRow) ()
updateStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "UPDATE league_team SET \
          \  league_config_id = $2, \
          \  team_key         = $3, \
          \  name             = $4, \
          \  owner            = $5, \
          \  updated_at       = NOW() \
          \WHERE id = $1"
    encoder = (fst >$< encDbLeagueTeamId) <> (snd >$< insertEncoder)

selectByIdStmt :: Stmt.Statement DbLeagueTeamId (Maybe LeagueTeamRow)
selectByIdStmt = Stmt.Statement sql encDbLeagueTeamId (D.rowMaybe rowDecoder) True
  where
    sql = "SELECT id, league_config_id, team_key, name, owner \
          \FROM league_team WHERE id = $1"

selectByKeyStmt :: Stmt.Statement (DbLeagueConfigId, Text) (Maybe LeagueTeamRow)
selectByKeyStmt = Stmt.Statement sql encoder (D.rowMaybe rowDecoder) True
  where
    sql = "SELECT id, league_config_id, team_key, name, owner \
          \FROM league_team \
          \WHERE league_config_id = $1 AND team_key = $2"
    encoder = (fst >$< encDbLeagueConfigId) <> (snd >$< encText)

selectForLeagueStmt :: Stmt.Statement DbLeagueConfigId (V.Vector LeagueTeamRow)
selectForLeagueStmt = Stmt.Statement sql encDbLeagueConfigId (D.rowVector rowDecoder) True
  where
    sql = "SELECT id, league_config_id, team_key, name, owner \
          \FROM league_team \
          \WHERE league_config_id = $1 \
          \ORDER BY name"

deleteStmt :: Stmt.Statement DbLeagueTeamId ()
deleteStmt = Stmt.Statement sql encDbLeagueTeamId D.noResult True
  where
    sql = "DELETE FROM league_team WHERE id = $1"