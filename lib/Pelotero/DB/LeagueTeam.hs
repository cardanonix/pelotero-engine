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

module Pelotero.DB.LeagueTeam
  ( LeagueTeamRow(..)
  , insertLeagueTeamT
  , updateLeagueTeamT
  , getByIdT
  , lookupByKeyT
  , getForLeagueT
  , deleteT
  , insertLeagueTeam
  , updateLeagueTeam
  , getById
  , lookupByKey
  , getForLeague
  , delete
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

import qualified Hasql.Transaction          as Tx

import           Rel8                       ( Column
                                            , Name
                                            , Rel8able
                                            , Result
                                            , TableSchema(..)
                                            , (==.)
                                            , (&&.)
                                            )
import qualified Rel8                       as R

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id    (DbLeagueConfigId(..), DbLeagueTeamId(..))

-- ============================================================================
-- league_team
-- ============================================================================

data LeagueTeam f = LeagueTeam
  { _ltId             :: Column f DbLeagueTeamId
  , _ltLeagueConfigId :: Column f DbLeagueConfigId
  , _ltTeamKey        :: Column f Text
  , _ltName           :: Column f Text
  , _ltOwner          :: Column f Text
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (LeagueTeam f)
deriving stock instance f ~ Result => Eq   (LeagueTeam f)

leagueTeamSchema :: TableSchema (LeagueTeam Name)
leagueTeamSchema = TableSchema
  { name    = "league_team"
  , columns = LeagueTeam
      { _ltId             = "id"
      , _ltLeagueConfigId = "league_config_id"
      , _ltTeamKey        = "team_key"
      , _ltName           = "name"
      , _ltOwner          = "owner"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

data LeagueTeamRow = LeagueTeamRow
  { ltId             :: !(Maybe DbLeagueTeamId)
  , ltLeagueConfigId :: !DbLeagueConfigId
  , ltTeamKey        :: !Text
  , ltName           :: !Text
  , ltOwner          :: !Text
  }
  deriving stock (Show, Eq)

fromResult :: LeagueTeam Result -> LeagueTeamRow
fromResult LeagueTeam{..} = LeagueTeamRow
  { ltId             = Just _ltId
  , ltLeagueConfigId = _ltLeagueConfigId
  , ltTeamKey        = _ltTeamKey
  , ltName           = _ltName
  , ltOwner          = _ltOwner
  }

-- ============================================================================
-- Transaction-flavored CRUD
-- ============================================================================

insertLeagueTeamT :: LeagueTeamRow -> Tx.Transaction DbLeagueTeamId
insertLeagueTeamT row = Tx.statement () $ R.run1 $ R.insert R.Insert
  { R.into       = leagueTeamSchema
  , R.rows       = R.values
      [ LeagueTeam
          { _ltId             = R.unsafeDefault
          , _ltLeagueConfigId = R.lit (ltLeagueConfigId row)
          , _ltTeamKey        = R.lit (ltTeamKey row)
          , _ltName           = R.lit (ltName row)
          , _ltOwner          = R.lit (ltOwner row)
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.Returning _ltId
  }

updateLeagueTeamT :: DbLeagueTeamId -> LeagueTeamRow -> Tx.Transaction ()
updateLeagueTeamT ltid row = Tx.statement () $ R.run_ $ R.update R.Update
  { R.target      = leagueTeamSchema
  , R.from        = pure ()
  , R.set         = \_ t -> t
      { _ltLeagueConfigId = R.lit (ltLeagueConfigId row)
      , _ltTeamKey        = R.lit (ltTeamKey row)
      , _ltName           = R.lit (ltName row)
      , _ltOwner          = R.lit (ltOwner row)
      }
  , R.updateWhere = \_ t -> _ltId t ==. R.lit ltid
  , R.returning   = R.NoReturning
  }

getByIdT :: DbLeagueTeamId -> Tx.Transaction (Maybe LeagueTeamRow)
getByIdT ltid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    t <- R.each leagueTeamSchema
    R.where_ (_ltId t ==. R.lit ltid)
    pure t
  pure $ case rows of
    (t : _) -> Just (fromResult t)
    []      -> Nothing

lookupByKeyT
  :: DbLeagueConfigId -> Text -> Tx.Transaction (Maybe LeagueTeamRow)
lookupByKeyT lcid key = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    t <- R.each leagueTeamSchema
    R.where_ (_ltLeagueConfigId t ==. R.lit lcid &&. _ltTeamKey t ==. R.lit key)
    pure t
  pure $ case rows of
    (t : _) -> Just (fromResult t)
    []      -> Nothing

getForLeagueT :: DbLeagueConfigId -> Tx.Transaction [LeagueTeamRow]
getForLeagueT lcid = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_ltName >$< R.asc) $ do
      t <- R.each leagueTeamSchema
      R.where_ (_ltLeagueConfigId t ==. R.lit lcid)
      pure t
  pure (map fromResult rows)

deleteT :: DbLeagueTeamId -> Tx.Transaction ()
deleteT ltid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = leagueTeamSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ t -> _ltId t ==. R.lit ltid
  , R.returning   = R.NoReturning
  }

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

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