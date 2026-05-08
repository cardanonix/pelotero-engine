{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Effects.BoxscoreEntry
  ( BoxscoreEntry (..)
    -- * Operations
  , upsertBatting
  , upsertPitching
  , getBattingForGame
  , getPitchingForGame
  , deleteBattingForGame
  , deletePitchingForGame
    -- * Interpreters
  , runBoxscoreEntryDB
  , runBoxscoreEntryNever
  ) where

import Effectful
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret, send)

import Pelotero.DB.BoxscoreEntry (BattingRow, PitchingRow)
import qualified Pelotero.DB.BoxscoreEntry as BoxRepo
import Pelotero.Domain.Id (DbGameId)
import Pelotero.Effects.Database (Database, runTx)

-- | The boxscore-entry effect: per-game upserts, reads, and deletes
-- for batting and pitching rows.
data BoxscoreEntry :: Effect where
  UpsertBatting         :: BattingRow  -> BoxscoreEntry m ()
  UpsertPitching        :: PitchingRow -> BoxscoreEntry m ()
  GetBattingForGame     :: DbGameId    -> BoxscoreEntry m [BattingRow]
  GetPitchingForGame    :: DbGameId    -> BoxscoreEntry m [PitchingRow]
  DeleteBattingForGame  :: DbGameId    -> BoxscoreEntry m ()
  DeletePitchingForGame :: DbGameId    -> BoxscoreEntry m ()

type instance DispatchOf BoxscoreEntry = Dynamic

upsertBatting :: BoxscoreEntry E.:> es => BattingRow -> E.Eff es ()
upsertBatting = send . UpsertBatting

upsertPitching :: BoxscoreEntry E.:> es => PitchingRow -> E.Eff es ()
upsertPitching = send . UpsertPitching

getBattingForGame
  :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es [BattingRow]
getBattingForGame = send . GetBattingForGame

getPitchingForGame
  :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es [PitchingRow]
getPitchingForGame = send . GetPitchingForGame

deleteBattingForGame :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es ()
deleteBattingForGame = send . DeleteBattingForGame

deletePitchingForGame :: BoxscoreEntry E.:> es => DbGameId -> E.Eff es ()
deletePitchingForGame = send . DeletePitchingForGame

-- | Production interpreter. Each operation runs in its own DB
-- transaction via the 'Database' effect; the 'Database' interpreter
-- is responsible for threading 'Error DBError' (post Phase A.3).
runBoxscoreEntryDB
  :: Database E.:> es
  => E.Eff (BoxscoreEntry : es) a
  -> E.Eff es a
runBoxscoreEntryDB = interpret $ \_ -> \case
  UpsertBatting br          -> runTx (BoxRepo.upsertBattingT br)
  UpsertPitching pr         -> runTx (BoxRepo.upsertPitchingT pr)
  GetBattingForGame gid     -> runTx (BoxRepo.getBattingForGameT gid)
  GetPitchingForGame gid    -> runTx (BoxRepo.getPitchingForGameT gid)
  DeleteBattingForGame gid  -> runTx (BoxRepo.deleteBattingForGameT gid)
  DeletePitchingForGame gid -> runTx (BoxRepo.deletePitchingForGameT gid)

-- | Test interpreter that errors on every operation. Useful for tests
-- that need 'BoxscoreEntry' present in the effect stack but exercise
-- paths where no batting / pitching row should be touched (e.g. the
-- C.1 SHA-skip tests with empty boxscore JSON).
--
-- Any actual invocation indicates the test took a path it wasn't
-- supposed to; the error message names the operation that fired so
-- the failing test points at the right place.
runBoxscoreEntryNever
  :: E.Eff (BoxscoreEntry : es) a
  -> E.Eff es a
runBoxscoreEntryNever = interpret $ \_ -> \case
  UpsertBatting _         ->
    error "runBoxscoreEntryNever: UpsertBatting was unexpectedly invoked"
  UpsertPitching _        ->
    error "runBoxscoreEntryNever: UpsertPitching was unexpectedly invoked"
  GetBattingForGame _     ->
    error "runBoxscoreEntryNever: GetBattingForGame was unexpectedly invoked"
  GetPitchingForGame _    ->
    error "runBoxscoreEntryNever: GetPitchingForGame was unexpectedly invoked"
  DeleteBattingForGame _  ->
    error "runBoxscoreEntryNever: DeleteBattingForGame was unexpectedly invoked"
  DeletePitchingForGame _ ->
    error "runBoxscoreEntryNever: DeletePitchingForGame was unexpectedly invoked"