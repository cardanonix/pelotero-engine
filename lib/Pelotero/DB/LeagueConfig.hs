-- | Repository for the @league_config@ table.
module Pelotero.DB.LeagueConfig
  ( -- * Row type
    LeagueConfigRow(..)
    -- * Transaction-level API
  , insertLeagueConfigT
  , updateLeagueConfigT
  , getByIdT
  , getByLeagueIdT
  , getAllT
    -- * Pool/IO API
  , insertLeagueConfig
  , updateLeagueConfig
  , getById
  , getByLeagueId
  , getAll
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text                  (Text)
import Data.Time                  (UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Statement
import Pelotero.Domain.Id    (DbLeagueConfigId(..))
import Pelotero.Domain.Roster (RosterLimits, LineupLimits)
import Pelotero.Domain.Scoring (LeagueScoring)

--------------------------------------------------------------------------------
-- Row type

data LeagueConfigRow = LeagueConfigRow
  { lcId            :: !(Maybe DbLeagueConfigId)
  , lcLeagueId      :: !Text
  , lcCommissioner  :: !Text
  , lcStatus        :: !Text
  , lcScoring       :: !LeagueScoring
  , lcRosterLimits  :: !RosterLimits
  , lcLineupLimits  :: !LineupLimits
  , lcDraftAuto     :: !Bool
  , lcDraftStrategy :: !Text
  , lcDraftAutoAt   :: !(Maybe UTCTime)
  , lcScoringStart  :: !UTCTime
  , lcScoringEnd    :: !UTCTime
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

insertLeagueConfigT :: LeagueConfigRow -> Tx.Transaction DbLeagueConfigId
insertLeagueConfigT row = Tx.statement row insertStmt

updateLeagueConfigT :: DbLeagueConfigId -> LeagueConfigRow -> Tx.Transaction ()
updateLeagueConfigT lcid row = Tx.statement (lcid, row) updateStmt

getByIdT :: DbLeagueConfigId -> Tx.Transaction (Maybe LeagueConfigRow)
getByIdT lcid = Tx.statement lcid selectByIdStmt

getByLeagueIdT :: Text -> Tx.Transaction (Maybe LeagueConfigRow)
getByLeagueIdT lid = Tx.statement lid selectByLeagueIdStmt

getAllT :: Tx.Transaction [LeagueConfigRow]
getAllT = V.toList <$> Tx.statement () selectAllStmt

--------------------------------------------------------------------------------
-- Pool/IO API

insertLeagueConfig :: Pool -> LeagueConfigRow -> IO (Either DBError DbLeagueConfigId)
insertLeagueConfig pool row = runTransaction pool (insertLeagueConfigT row)

updateLeagueConfig :: Pool -> DbLeagueConfigId -> LeagueConfigRow -> IO (Either DBError ())
updateLeagueConfig pool lcid row = runTransaction pool (updateLeagueConfigT lcid row)

getById :: Pool -> DbLeagueConfigId -> IO (Either DBError (Maybe LeagueConfigRow))
getById pool lcid = runTransaction pool (getByIdT lcid)

getByLeagueId :: Pool -> Text -> IO (Either DBError (Maybe LeagueConfigRow))
getByLeagueId pool lid = runTransaction pool (getByLeagueIdT lid)

getAll :: Pool -> IO (Either DBError [LeagueConfigRow])
getAll pool = runTransaction pool getAllT

--------------------------------------------------------------------------------
-- Encoder

insertEncoder :: E.Params LeagueConfigRow
insertEncoder =
     (lcLeagueId      >$< encText)
  <> (lcCommissioner  >$< encText)
  <> (lcStatus        >$< encText)
  <> (lcScoring       >$< encJsonb)
  <> (lcRosterLimits  >$< encJsonb)
  <> (lcLineupLimits  >$< encJsonb)
  <> (lcDraftAuto     >$< encBool)
  <> (lcDraftStrategy >$< encText)
  <> (lcDraftAutoAt   >$< encUTCTimeMaybe)
  <> (lcScoringStart  >$< encUTCTime)
  <> (lcScoringEnd    >$< encUTCTime)

--------------------------------------------------------------------------------
-- Decoder

rowDecoder :: D.Row LeagueConfigRow
rowDecoder = LeagueConfigRow
  <$> (Just <$> decDbLeagueConfigId)
  <*> decText
  <*> decText
  <*> decText
  <*> decJsonb
  <*> decJsonb
  <*> decJsonb
  <*> decBool
  <*> decText
  <*> decUTCTimeMaybe
  <*> decUTCTime
  <*> decUTCTime

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement LeagueConfigRow DbLeagueConfigId
insertStmt = Stmt.Statement sql insertEncoder (D.singleRow decDbLeagueConfigId) True
  where
    sql = "INSERT INTO league_config \
          \  (league_id, commissioner, status, scoring_config, \
          \   roster_limits, lineup_limits, draft_auto, draft_strategy, \
          \   draft_auto_at, scoring_start, scoring_end) \
          \VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11) \
          \RETURNING id"

updateStmt :: Stmt.Statement (DbLeagueConfigId, LeagueConfigRow) ()
updateStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "UPDATE league_config SET \
          \  league_id      = $2, \
          \  commissioner   = $3, \
          \  status         = $4, \
          \  scoring_config = $5, \
          \  roster_limits  = $6, \
          \  lineup_limits  = $7, \
          \  draft_auto     = $8, \
          \  draft_strategy = $9, \
          \  draft_auto_at  = $10, \
          \  scoring_start  = $11, \
          \  scoring_end    = $12, \
          \  updated_at     = NOW() \
          \WHERE id = $1"
    encoder = (fst >$< encDbLeagueConfigId) <> (snd >$< insertEncoder)

selectByIdStmt :: Stmt.Statement DbLeagueConfigId (Maybe LeagueConfigRow)
selectByIdStmt = Stmt.Statement sql encDbLeagueConfigId (D.rowMaybe rowDecoder) True
  where
    sql = "SELECT id, league_id, commissioner, status, \
          \       scoring_config, roster_limits, lineup_limits, \
          \       draft_auto, draft_strategy, draft_auto_at, \
          \       scoring_start, scoring_end \
          \FROM league_config WHERE id = $1"

selectByLeagueIdStmt :: Stmt.Statement Text (Maybe LeagueConfigRow)
selectByLeagueIdStmt = Stmt.Statement sql encText (D.rowMaybe rowDecoder) True
  where
    sql = "SELECT id, league_id, commissioner, status, \
          \       scoring_config, roster_limits, lineup_limits, \
          \       draft_auto, draft_strategy, draft_auto_at, \
          \       scoring_start, scoring_end \
          \FROM league_config WHERE league_id = $1"

selectAllStmt :: Stmt.Statement () (V.Vector LeagueConfigRow)
selectAllStmt = Stmt.Statement sql E.noParams (D.rowVector rowDecoder) True
  where
    sql = "SELECT id, league_id, commissioner, status, \
          \       scoring_config, roster_limits, lineup_limits, \
          \       draft_auto, draft_strategy, draft_auto_at, \
          \       scoring_start, scoring_end \
          \FROM league_config \
          \ORDER BY league_id"