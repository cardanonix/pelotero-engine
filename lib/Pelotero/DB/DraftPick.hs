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

module Pelotero.DB.DraftPick
  ( DraftPickRow(..)
  , recordPickT
  , getPicksForLeagueT
  , getPickCountT
  , recordPick
  , getPicksForLeague
  , getPickCount
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Int                   (Int32, Int64)
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

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id
  ( DbDraftPickId(..)
  , DbLeagueConfigId(..)
  , DbLeagueTeamId(..)
  , DbPlayerId(..)
  )

-- ============================================================================
-- draft_pick
-- ============================================================================

data DraftPickE f = DraftPickE
  { _dpId             :: Column f DbDraftPickId
  , _dpLeagueConfigId :: Column f DbLeagueConfigId
  , _dpPickNumber     :: Column f Int32
  , _dpLeagueTeamId   :: Column f DbLeagueTeamId
  , _dpPlayerId       :: Column f DbPlayerId
  , _dpPickedAt       :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (DraftPickE f)
deriving stock instance f ~ Result => Eq   (DraftPickE f)

draftPickSchema :: TableSchema (DraftPickE Name)
draftPickSchema = TableSchema
  { name    = "draft_pick"
  , columns = DraftPickE
      { _dpId             = "id"
      , _dpLeagueConfigId = "league_config_id"
      , _dpPickNumber     = "pick_number"
      , _dpLeagueTeamId   = "league_team_id"
      , _dpPlayerId       = "player_id"
      , _dpPickedAt       = "picked_at"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

data DraftPickRow = DraftPickRow
  { dpId             :: !(Maybe DbDraftPickId)
  , dpLeagueConfigId :: !DbLeagueConfigId
  , dpPickNumber     :: !Int32
  , dpLeagueTeamId   :: !DbLeagueTeamId
  , dpPlayerId       :: !DbPlayerId
  , dpPickedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

fromResult :: DraftPickE Result -> DraftPickRow
fromResult DraftPickE{..} = DraftPickRow
  { dpId             = Just _dpId
  , dpLeagueConfigId = _dpLeagueConfigId
  , dpPickNumber     = _dpPickNumber
  , dpLeagueTeamId   = _dpLeagueTeamId
  , dpPlayerId       = _dpPlayerId
  , dpPickedAt       = Just _dpPickedAt
  }

-- ============================================================================
-- Transaction-flavored CRUD
-- ============================================================================

recordPickT :: DraftPickRow -> Tx.Transaction DbDraftPickId
recordPickT row = Tx.statement () $ R.run1 $ R.insert R.Insert
  { R.into       = draftPickSchema
  , R.rows       = R.values
      [ DraftPickE
          { _dpId             = R.unsafeDefault
          , _dpLeagueConfigId = R.lit (dpLeagueConfigId row)
          , _dpPickNumber     = R.lit (dpPickNumber row)
          , _dpLeagueTeamId   = R.lit (dpLeagueTeamId row)
          , _dpPlayerId       = R.lit (dpPlayerId row)
          , _dpPickedAt       = R.unsafeDefault
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.Returning _dpId
  }

getPicksForLeagueT :: DbLeagueConfigId -> Tx.Transaction [DraftPickRow]
getPicksForLeagueT lcid = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_dpPickNumber >$< R.asc) $ do
      d <- R.each draftPickSchema
      R.where_ (_dpLeagueConfigId d ==. R.lit lcid)
      pure d
  pure (map fromResult rows)

getPickCountT :: DbLeagueConfigId -> Tx.Transaction Int64
getPickCountT lcid = do
  ns <- Tx.statement () $ R.run $ R.select $ R.aggregate1 R.countStar $ do
    d <- R.each draftPickSchema
    R.where_ (_dpLeagueConfigId d ==. R.lit lcid)
    pure d
  pure $ case ns of
    (n : _) -> n
    []      -> 0

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

recordPick :: Pool -> DraftPickRow -> IO (Either DBError DbDraftPickId)
recordPick pool row = runTransaction pool (recordPickT row)

getPicksForLeague :: Pool -> DbLeagueConfigId -> IO (Either DBError [DraftPickRow])
getPicksForLeague pool lcid = runTransaction pool (getPicksForLeagueT lcid)

getPickCount :: Pool -> DbLeagueConfigId -> IO (Either DBError Int64)
getPickCount pool lcid = runTransaction pool (getPickCountT lcid)