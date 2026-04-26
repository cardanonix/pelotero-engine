{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}

-- | The 'Players' capability: an algebra over the operations sync code
-- needs to perform on player records, without committing to a specific
-- backend.
--
-- Two interpreters are provided:
--
--   * 'runPlayersDB' — backed by 'Pelotero.DB.Player' and the connection
--     pool from 'Pelotero.Effects.DbPool'. The production path.
--
--   * 'runPlayersInMemory' — backed by an 'IORef' holding a 'Map'. Used
--     in tests; no PostgreSQL required.
--
-- Each operation in the capability is a constructor of 'Players'; the
-- helper functions ('upsertPlayerByExternalId', 'getPlayerById', etc.)
-- are thin wrappers around 'send'. We write them by hand rather than
-- generating with @makeEffect@ because each one gets its own Haddock
-- describing the semantic contract — separate from the constructor's
-- mechanical type.
module Pelotero.Effects.Players
  ( -- * Capability
    Players(..)
    -- * Operations
  , upsertPlayerByExternalId
  , getPlayerById
  , getActivePlayers
    -- * Interpreters
  , runPlayersDB
  , runPlayersInMemory
  ) where

import Data.IORef             (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text              (Text)

import Effectful (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import Pelotero.DB.Pool      (DBError, runTransaction)
import Pelotero.DB.Player    (PlayerRow(..))
import qualified Pelotero.DB.Player as PlayerRepo
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.Domain.Id    (DbPlayerId(..))
import Pelotero.Effects.DbPool (DbPool, getPool)

--------------------------------------------------------------------------------
-- Capability

-- | Operations sync code performs on player records.
--
-- /Semantic notes:/
--
--   * 'UpsertPlayerByExternalId' is atomic — if it fails, no row is left
--     in a partial state. The 'DbPlayerId' returned is the surrogate the
--     caller should use to attach related rows (e.g. stat entries).
--
--   * 'GetPlayerById' returns 'Nothing' for unknown ids; it does not
--     throw. Callers handle the missing case explicitly.
--
--   * 'GetActivePlayers' returns rows in stable name order (last, first).
data Players :: Effect where
  UpsertPlayerByExternalId :: ProviderName -> Text -> PlayerRow -> Players m DbPlayerId
  GetPlayerById            :: DbPlayerId -> Players m (Maybe PlayerRow)
  GetActivePlayers         :: Players m [PlayerRow]

type instance DispatchOf Players = 'Dynamic

--------------------------------------------------------------------------------
-- Operations

-- | Insert or update a player keyed on @(provider, external_id)@. Returns
-- the surrogate id, whether newly created or pre-existing.
upsertPlayerByExternalId
  :: Players E.:> es
  => ProviderName
  -> Text                 -- ^ external id
  -> PlayerRow
  -> E.Eff es DbPlayerId
upsertPlayerByExternalId provider extId row =
  send (UpsertPlayerByExternalId provider extId row)

-- | Fetch a player by surrogate id. 'Nothing' if no such player.
getPlayerById :: Players E.:> es => DbPlayerId -> E.Eff es (Maybe PlayerRow)
getPlayerById = send . GetPlayerById

-- | All players whose @active@ flag is true, ordered by last name then
-- first name.
getActivePlayers :: Players E.:> es => E.Eff es [PlayerRow]
getActivePlayers = send GetActivePlayers

--------------------------------------------------------------------------------
-- DB-backed interpreter

-- | The production interpreter. Pulls the pool from 'DbPool' and runs
-- repository operations against it. Every operation maps to one
-- 'runTransaction' — no batching at this layer.
runPlayersDB
  :: (IOE E.:> es, DbPool E.:> es)
  => E.Eff (Players : es) a
  -> E.Eff es a
runPlayersDB = interpret_ $ \case
  UpsertPlayerByExternalId provider extId row -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (PlayerRepo.upsertByExternalIdT provider extId row)

  GetPlayerById pid -> do
    pool <- getPool
    runOrThrow $ runTransaction pool (PlayerRepo.getByIdT pid)

  GetActivePlayers -> do
    pool <- getPool
    runOrThrow $ runTransaction pool PlayerRepo.getActiveT
  where
    -- | Lift @IO (Either DBError a)@ into 'Eff', throwing on the @Left@.
    --
    -- We throw rather than return 'Either' because at this layer the
    -- caller has no actionable response to a DB failure — the application
    -- crashes (and gets restarted by systemd, or whatever's running it).
    -- A future revision could add a structured 'Error DBError' effect for
    -- finer-grained recovery; not needed yet.
    runOrThrow :: IOE E.:> es' => IO (Either DBError a) -> E.Eff es' a
    runOrThrow io = E.liftIO io >>= \case
      Right a  -> pure a
      Left err -> E.liftIO (ioError (userError ("DB error: " <> show err)))

--------------------------------------------------------------------------------
-- In-memory interpreter

-- | An in-memory store. Used in tests.
--
-- Maps:
--   * 'externalIdToDb' — provider+external_id → surrogate, for upsert
--     resolution
--   * 'rowsByDb' — surrogate → row, for reads
--   * 'nextId' — counter for assigning new surrogates
--
-- A real production implementation would not be this simple (no
-- concurrency, no persistence); this is for testing only.
data PlayerStore = PlayerStore
  { externalIdToDb :: !(Map.Map (ProviderName, Text) DbPlayerId)
  , rowsByDb       :: !(Map.Map DbPlayerId PlayerRow)
  , nextId         :: !Int
  }

emptyStore :: PlayerStore
emptyStore = PlayerStore Map.empty Map.empty 1

-- | Run 'Players' against an in-memory store. The store starts empty and
-- is discarded when the action returns.
runPlayersInMemory
  :: IOE E.:> es
  => E.Eff (Players : es) a
  -> E.Eff es a
runPlayersInMemory action = do
  ref <- E.liftIO (newIORef emptyStore)
  interpret_ (handler ref) action
  where
    handler :: IOE E.:> es => IORef PlayerStore -> Players m b -> E.Eff es b
    handler ref = \case
      UpsertPlayerByExternalId provider extId row ->
        E.liftIO $ atomicModifyIORef' ref (upsertOp provider extId row)

      GetPlayerById pid -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup pid (rowsByDb store))

      GetActivePlayers -> do
        store <- E.liftIO (readIORef ref)
        pure
          $ filter playerRowActive
          $ map snd
          $ Map.toList (rowsByDb store)

    upsertOp
      :: ProviderName
      -> Text
      -> PlayerRow
      -> PlayerStore
      -> (PlayerStore, DbPlayerId)
    upsertOp provider extId incoming store =
      case Map.lookup (provider, extId) (externalIdToDb store) of
        Just pid ->
          -- Update existing row, preserving its DbPlayerId.
          let updated = incoming { playerRowId = Just pid }
              store' = store
                { rowsByDb = Map.insert pid updated (rowsByDb store) }
          in (store', pid)
        Nothing ->
          -- Insert: assign a fresh id, link the external id, store the row.
          let pid     = DbPlayerId (fromIntegral (nextId store))
              stored  = incoming { playerRowId = Just pid }
              store'  = PlayerStore
                { externalIdToDb = Map.insert (provider, extId) pid (externalIdToDb store)
                , rowsByDb       = Map.insert pid stored (rowsByDb store)
                , nextId         = nextId store + 1
                }
          in (store', pid)