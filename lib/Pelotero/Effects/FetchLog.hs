{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}

module Pelotero.Effects.FetchLog
  ( FetchLog(..)
  , recordFetch
  , getLastFetch
  , runFetchLogDB
  , runFetchLogInMemory
  ) where

import Data.IORef            (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text             (Text)

import Effectful (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import Pelotero.DB.FetchLog  (FetchLogRow(..))
import qualified Pelotero.DB.FetchLog as FetchLogRepo
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.Effects.Database (Database, runTx)

data FetchLog :: Effect where
  RecordFetch  :: FetchLogRow -> FetchLog m ()
  GetLastFetch :: ProviderName -> Text -> Text -> FetchLog m (Maybe FetchLogRow)

type instance DispatchOf FetchLog = 'Dynamic

recordFetch :: FetchLog E.:> es => FetchLogRow -> E.Eff es ()
recordFetch = send . RecordFetch

getLastFetch
  :: FetchLog E.:> es
  => ProviderName
  -> Text
  -> Text
  -> E.Eff es (Maybe FetchLogRow)
getLastFetch p r s = send (GetLastFetch p r s)

runFetchLogDB
  :: Database E.:> es
  => E.Eff (FetchLog : es) a
  -> E.Eff es a
runFetchLogDB = interpret_ $ \case
  RecordFetch row ->
    runTx (FetchLogRepo.recordFetchT row)
  GetLastFetch provider resource scope ->
    runTx (FetchLogRepo.getLastFetchT provider resource scope)

runFetchLogInMemory
  :: IOE E.:> es
  => E.Eff (FetchLog : es) a
  -> E.Eff es a
runFetchLogInMemory action = do
  ref <- E.liftIO (newIORef [])
  interpret_ (handler ref) action
  where
    handler :: IOE E.:> es => IORef [FetchLogRow] -> FetchLog m b -> E.Eff es b
    handler ref = \case
      RecordFetch row ->
        E.liftIO $ atomicModifyIORef' ref (\xs -> (row : xs, ()))
      GetLastFetch provider resource scope -> do
        rows <- E.liftIO (readIORef ref)
        pure $ case filter (matches provider resource scope) rows of
          (x:_) -> Just x
          []    -> Nothing

    matches p r s row =
      fetchLogProvider row == p
        && fetchLogResource row == r
        && fetchLogScope    row == s