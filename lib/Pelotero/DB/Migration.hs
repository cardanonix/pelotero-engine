-- | Schema migrations.
--
-- A from-scratch implementation. We previously used @hasql-migration@, but
-- it's broken against current @crypton@ and effectively unmaintained.
-- Migrations are simple enough that owning the implementation is cheaper
-- than maintaining a fork.
--
-- The model:
--
--   * @.sql@ files live in a directory; lexicographic order is apply order.
--     The convention is @V<NNNN>__<description>.sql@, e.g.
--     @V0001__init.sql@.
--   * Each file is identified by its filename and protected by a SHA-256
--     of its contents. The first time a filename is applied, both go into
--     the @schema_migrations@ table. On subsequent runs:
--
--       - If the filename is present and the hash matches, it's skipped.
--       - If the filename is present and the hash differs, we abort with
--         'MigrationChanged' — applied migrations are immutable; if you
--         need a change, write a new migration.
--       - If the filename is absent, we apply the file in a single
--         transaction along with the @schema_migrations@ insert. Either
--         the migration applies and is recorded, or it rolls back; we
--         never end up half-applied.
--
-- The whole-file SHA-256 is overkill for the protection it provides
-- (whitespace changes count as edits). That's the point — operational
-- guardrails should be picky.
module Pelotero.DB.Migration
  ( runMigrations
  , MigrationOutcome(..)
  ) where

import Control.Monad           (foldM)
import Crypto.Hash.SHA256      (hash)
import Data.ByteString         (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Functor.Contravariant (contramap)
import Data.List               (sort)
import Data.Text               (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Decoders            as D
import qualified Hasql.Encoders            as E
import qualified Hasql.Session             as Session
import qualified Hasql.Statement           as Stmt
import qualified Hasql.Transaction         as Tx
import qualified Hasql.Transaction.Sessions as TxS
import System.Directory        (doesDirectoryExist, listDirectory)
import System.FilePath         ((</>), takeExtension)

import Pelotero.DB.Pool (DBError(..), Pool, runSession)

--------------------------------------------------------------------------------
-- Public API

-- | Summary of a migration run.
data MigrationOutcome = MigrationOutcome
  { migrationsTotalSeen  :: !Int  -- ^ files examined
  , migrationsAppliedNow :: !Int  -- ^ first-time applies during this call
  , migrationsAlreadyApplied :: !Int -- ^ skipped because already applied
  }
  deriving stock (Show, Eq)

-- | Apply any unapplied migrations from @dir@. Idempotent and safe to call
-- on every process startup.
runMigrations :: Pool -> FilePath -> IO (Either DBError MigrationOutcome)
runMigrations pool dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure $ Left (MigrationError ("Migration directory not found: " <> T.pack dir))
    else do
      files   <- sort . filter ((== ".sql") . takeExtension) <$> listDirectory dir
      scripts <- traverse (loadScript dir) files
      run     <- runSession pool (applyAll scripts)
      pure $ case run of
        Left  poolErr  -> Left poolErr
        Right (Left e) -> Left (MigrationError e)
        Right (Right o) -> Right o

--------------------------------------------------------------------------------
-- Internal: file loading

loadScript :: FilePath -> FilePath -> IO Script
loadScript dir name = do
  contents <- BS.readFile (dir </> name)
  pure Script
    { scriptName     = T.pack name
    , scriptContents = contents
    , scriptChecksum = hexSha256 contents
    }

data Script = Script
  { scriptName     :: !Text
  , scriptContents :: !ByteString
  , scriptChecksum :: !Text
  }

hexSha256 :: ByteString -> Text
hexSha256 = TE.decodeUtf8 . B16.encode . hash

--------------------------------------------------------------------------------
-- Internal: orchestration

-- | Apply every script in turn. We run the @schema_migrations@ bootstrap
-- in its own transaction (it's an idempotent CREATE), then each migration
-- in its own transaction so a failure rolls back exactly the offending one
-- and not the whole batch.
applyAll :: [Script] -> Session.Session (Either Text MigrationOutcome)
applyAll scripts = do
  TxS.transaction TxS.ReadCommitted TxS.Write ensureMigrationsTable
  let initial = MigrationOutcome
        { migrationsTotalSeen      = length scripts
        , migrationsAppliedNow     = 0
        , migrationsAlreadyApplied = 0
        }
  foldM step (Right initial) scripts
  where
    step acc@(Left _) _ = pure acc
    step (Right outcome) script = do
      r <- TxS.transaction TxS.ReadCommitted TxS.Write (applyOne script)
      pure $ case r of
        Skipped   -> Right outcome { migrationsAlreadyApplied = migrationsAlreadyApplied outcome + 1 }
        Applied   -> Right outcome { migrationsAppliedNow     = migrationsAppliedNow     outcome + 1 }
        Mismatch  -> Left (mismatchError script)

    mismatchError s =
      "Migration " <> scriptName s
        <> " has changed since first apply (current sha256 = "
        <> scriptChecksum s
        <> "). Migrations are immutable; create a new file rather than editing."

data ApplyResult = Skipped | Applied | Mismatch

-- | Apply one migration, given that 'ensureMigrationsTable' has already run.
-- Looks up any prior application; if absent, runs the SQL and records it.
applyOne :: Script -> Tx.Transaction ApplyResult
applyOne s = do
  prior <- Tx.statement (scriptName s) selectChecksum
  case prior of
    Just c
      | c == scriptChecksum s -> pure Skipped
      | otherwise             -> pure Mismatch
    Nothing -> do
      Tx.sql (scriptContents s)
      Tx.statement (scriptName s, scriptChecksum s) insertApplied
      pure Applied

--------------------------------------------------------------------------------
-- Internal: SQL

ensureMigrationsTable :: Tx.Transaction ()
ensureMigrationsTable = Tx.sql
  "CREATE TABLE IF NOT EXISTS schema_migrations (\
  \  filename    TEXT        PRIMARY KEY,\
  \  checksum    TEXT        NOT NULL,\
  \  applied_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()\
  \)"

selectChecksum :: Stmt.Statement Text (Maybe Text)
selectChecksum =
  Stmt.Statement
    "SELECT checksum FROM schema_migrations WHERE filename = $1"
    (E.param (E.nonNullable E.text))
    (D.rowMaybe (D.column (D.nonNullable D.text)))
    True

insertApplied :: Stmt.Statement (Text, Text) ()
insertApplied =
  Stmt.Statement
    "INSERT INTO schema_migrations (filename, checksum) VALUES ($1, $2)"
    encoder
    D.noResult
    True
  where
    encoder =
         contramap fst (E.param (E.nonNullable E.text))
      <> contramap snd (E.param (E.nonNullable E.text))