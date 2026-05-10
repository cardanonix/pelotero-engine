{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pelotero.DB.LeagueConfig
  ( LeagueConfigRow(..)
  , LoadedLeagueConfig(..)
  , insertLeagueConfigT
  , updateLeagueConfigT
  , getByIdT
  , getByLeagueIdT
  , getAllT
  , insertLeagueConfig
  , updateLeagueConfig
  , getById
  , getByLeagueId
  , getAll
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)

import qualified Hasql.Transaction          as Tx

import           Rel8                       ( Column
                                            , Name
                                            , Rel8able
                                            , Result
                                            , TableSchema(..)
                                            , (==.)
                                            )
import qualified Rel8                       as R

import Pelotero.DB.JsonB
                     ( JsonbLineupLimits (..)
                     , JsonbRosterLimits (..)
                     , JsonbScoring (..)
                     )
import Pelotero.DB.Pool       (DBError, Pool, runTransaction)
import Pelotero.DB.Rel8Instances  ()
import Pelotero.Domain.Id     (DbLeagueConfigId(..))
import Pelotero.Domain.Roster (LineupLimits, RosterLimits)
import Pelotero.Domain.Scoring (LeagueScoring)

data LeagueConfig f = LeagueConfig
  { _lcId            :: Column f DbLeagueConfigId
  , _lcLeagueId      :: Column f Text
  , _lcCommissioner  :: Column f Text
  , _lcStatus        :: Column f Text
  , _lcScoring       :: Column f JsonbScoring
  , _lcRosterLimits  :: Column f JsonbRosterLimits
  , _lcLineupLimits  :: Column f JsonbLineupLimits
  , _lcDraftAuto     :: Column f Bool
  , _lcDraftStrategy :: Column f Text
  , _lcDraftAutoAt   :: Column f (Maybe UTCTime)
  , _lcScoringStart  :: Column f UTCTime
  , _lcScoringEnd    :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (LeagueConfig f)
deriving stock instance f ~ Result => Eq   (LeagueConfig f)

leagueConfigSchema :: TableSchema (LeagueConfig Name)
leagueConfigSchema = TableSchema
  { name    = "league_config"
  , columns = LeagueConfig
      { _lcId            = "id"
      , _lcLeagueId      = "league_id"
      , _lcCommissioner  = "commissioner"
      , _lcStatus        = "status"
      , _lcScoring       = "scoring_config"
      , _lcRosterLimits  = "roster_limits"
      , _lcLineupLimits  = "lineup_limits"
      , _lcDraftAuto     = "draft_auto"
      , _lcDraftStrategy = "draft_strategy"
      , _lcDraftAutoAt   = "draft_auto_at"
      , _lcScoringStart  = "scoring_start"
      , _lcScoringEnd    = "scoring_end"
      }
  }

-- | An unsaved or partially-known league config. Used for inserts and
-- updates, where the caller may not yet know the surrogate id.
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

-- | A league config loaded from the database. The id is always present;
-- partial functions on the read path are gone.
data LoadedLeagueConfig = LoadedLeagueConfig
  { llcId            :: !DbLeagueConfigId
  , llcLeagueId      :: !Text
  , llcCommissioner  :: !Text
  , llcStatus        :: !Text
  , llcScoring       :: !LeagueScoring
  , llcRosterLimits  :: !RosterLimits
  , llcLineupLimits  :: !LineupLimits
  , llcDraftAuto     :: !Bool
  , llcDraftStrategy :: !Text
  , llcDraftAutoAt   :: !(Maybe UTCTime)
  , llcScoringStart  :: !UTCTime
  , llcScoringEnd    :: !UTCTime
  }
  deriving stock (Show, Eq)

fromResult :: LeagueConfig Result -> LoadedLeagueConfig
fromResult LeagueConfig{..} = LoadedLeagueConfig
  { llcId            = _lcId
  , llcLeagueId      = _lcLeagueId
  , llcCommissioner  = _lcCommissioner
  , llcStatus        = _lcStatus
  , llcScoring       = unJsonbScoring      _lcScoring
  , llcRosterLimits  = unJsonbRosterLimits _lcRosterLimits
  , llcLineupLimits  = unJsonbLineupLimits _lcLineupLimits
  , llcDraftAuto     = _lcDraftAuto
  , llcDraftStrategy = _lcDraftStrategy
  , llcDraftAutoAt   = _lcDraftAutoAt
  , llcScoringStart  = _lcScoringStart
  , llcScoringEnd    = _lcScoringEnd
  }

insertLeagueConfigT :: LeagueConfigRow -> Tx.Transaction DbLeagueConfigId
insertLeagueConfigT row = Tx.statement () $ R.run1 $ R.insert R.Insert
  { R.into       = leagueConfigSchema
  , R.rows       = R.values
      [ LeagueConfig
          { _lcId            = R.unsafeDefault
          , _lcLeagueId      = R.lit (lcLeagueId row)
          , _lcCommissioner  = R.lit (lcCommissioner row)
          , _lcStatus        = R.lit (lcStatus row)
          , _lcScoring       = R.lit (JsonbScoring      (lcScoring       row))
          , _lcRosterLimits  = R.lit (JsonbRosterLimits (lcRosterLimits  row))
          , _lcLineupLimits  = R.lit (JsonbLineupLimits (lcLineupLimits  row))
          , _lcDraftAuto     = R.lit (lcDraftAuto row)
          , _lcDraftStrategy = R.lit (lcDraftStrategy row)
          , _lcDraftAutoAt   = R.lit (lcDraftAutoAt row)
          , _lcScoringStart  = R.lit (lcScoringStart row)
          , _lcScoringEnd    = R.lit (lcScoringEnd row)
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.Returning _lcId
  }

updateLeagueConfigT :: DbLeagueConfigId -> LeagueConfigRow -> Tx.Transaction ()
updateLeagueConfigT lcid row = Tx.statement () $ R.run_ $ R.update R.Update
  { R.target      = leagueConfigSchema
  , R.from        = pure ()
  , R.set         = \_ c -> c
      { _lcLeagueId      = R.lit (lcLeagueId row)
      , _lcCommissioner  = R.lit (lcCommissioner row)
      , _lcStatus        = R.lit (lcStatus row)
      , _lcScoring       = R.lit (JsonbScoring      (lcScoring       row))
      , _lcRosterLimits  = R.lit (JsonbRosterLimits (lcRosterLimits  row))
      , _lcLineupLimits  = R.lit (JsonbLineupLimits (lcLineupLimits  row))
      , _lcDraftAuto     = R.lit (lcDraftAuto row)
      , _lcDraftStrategy = R.lit (lcDraftStrategy row)
      , _lcDraftAutoAt   = R.lit (lcDraftAutoAt row)
      , _lcScoringStart  = R.lit (lcScoringStart row)
      , _lcScoringEnd    = R.lit (lcScoringEnd row)
      }
  , R.updateWhere = \_ c -> _lcId c ==. R.lit lcid
  , R.returning   = R.NoReturning
  }

getByIdT :: DbLeagueConfigId -> Tx.Transaction (Maybe LoadedLeagueConfig)
getByIdT lcid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    c <- R.each leagueConfigSchema
    R.where_ (_lcId c ==. R.lit lcid)
    pure c
  pure $ case rows of
    (c : _) -> Just (fromResult c)
    []      -> Nothing

getByLeagueIdT :: Text -> Tx.Transaction (Maybe LoadedLeagueConfig)
getByLeagueIdT lid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    c <- R.each leagueConfigSchema
    R.where_ (_lcLeagueId c ==. R.lit lid)
    pure c
  pure $ case rows of
    (c : _) -> Just (fromResult c)
    []      -> Nothing

getAllT :: Tx.Transaction [LoadedLeagueConfig]
getAllT = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_lcLeagueId >$< R.asc) (R.each leagueConfigSchema)
  pure (map fromResult rows)

insertLeagueConfig :: Pool -> LeagueConfigRow -> IO (Either DBError DbLeagueConfigId)
insertLeagueConfig pool row = runTransaction pool (insertLeagueConfigT row)

updateLeagueConfig :: Pool -> DbLeagueConfigId -> LeagueConfigRow -> IO (Either DBError ())
updateLeagueConfig pool lcid row = runTransaction pool (updateLeagueConfigT lcid row)

getById :: Pool -> DbLeagueConfigId -> IO (Either DBError (Maybe LoadedLeagueConfig))
getById pool lcid = runTransaction pool (getByIdT lcid)

getByLeagueId :: Pool -> Text -> IO (Either DBError (Maybe LoadedLeagueConfig))
getByLeagueId pool lid = runTransaction pool (getByLeagueIdT lid)

getAll :: Pool -> IO (Either DBError [LoadedLeagueConfig])
getAll pool = runTransaction pool getAllT