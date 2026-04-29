{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Pelotero.Effects.BoxscoreEntry
  ( BoxscoreEntry(..)
  , upsertBatting
  , upsertPitching
  , getBattingForGame
  , getPitchingForGame
  , deleteBattingForGame
  , deletePitchingForGame
  , runBoxscoreEntryDB
  ) where

import Effectful (Effect, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import qualified Pelotero.DB.BoxscoreEntry as BoxRepo
import           Pelotero.DB.BoxscoreEntry (BattingRow, PitchingRow)
import           Pelotero.Domain.Id        (DbGameId)
import           Pelotero.Effects.Database (Database, runTx)

data BoxscoreEntry :: Effect where
  UpsertBatting          :: BattingRow  -> BoxscoreEntry m ()
  UpsertPitching         :: PitchingRow -> BoxscoreEntry m ()
  GetBattingForGame      :: DbGameId    -> BoxscoreEntry m [BattingRow]
  GetPitchingForGame     :: DbGameId    -> BoxscoreEntry m [PitchingRow]
  DeleteBattingForGame   :: DbGameId    -> BoxscoreEntry m ()
  DeletePitchingForGame  :: DbGameId    -> BoxscoreEntry m ()

type instance DispatchOf BoxscoreEntry = 'Dynamic

upsertBatting :: BoxscoreEntry E.:> es => BattingRow -> E.Eff es ()
upsertBatting = send . UpsertBatting

upsertPitching :: BoxscoreEntry E.:> es => PitchingRow -> E.Eff es ()
upsertPitching = send . UpsertPitching

getBattingForGame :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es [BattingRow]
getBattingForGame = send . GetBattingForGame

getPitchingForGame :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es [PitchingRow]
getPitchingForGame = send . GetPitchingForGame

deleteBattingForGame :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es ()
deleteBattingForGame = send . DeleteBattingForGame

deletePitchingForGame :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es ()
deletePitchingForGame = send . DeletePitchingForGame

runBoxscoreEntryDB
  :: Database E.:> es
  => E.Eff (BoxscoreEntry : es) a
  -> E.Eff es a
runBoxscoreEntryDB = interpret_ $ \case
  UpsertBatting row         -> runTx (BoxRepo.upsertBattingT row)
  UpsertPitching row        -> runTx (BoxRepo.upsertPitchingT row)
  GetBattingForGame gid     -> runTx (BoxRepo.getBattingForGameT gid)
  GetPitchingForGame gid    -> runTx (BoxRepo.getPitchingForGameT gid)
  DeleteBattingForGame gid  -> runTx (BoxRepo.deleteBattingForGameT gid)
  DeletePitchingForGame gid -> runTx (BoxRepo.deletePitchingForGameT gid)