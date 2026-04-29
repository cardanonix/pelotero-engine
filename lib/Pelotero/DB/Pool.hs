-- | Database connection pool plus runtime configuration.
--
-- Reads connection parameters from libpq's standard environment variables
-- (@PGHOST@, @PGPORT@, @PGDATABASE@, @PGUSER@, @PGPASSWORD@), set by the
-- dev shell or by sops-exec at process start. Two pelotero-specific
-- variables tune pool behaviour: @PELOTERO_DB_POOL_SIZE@ (max connections)
-- and @PELOTERO_DB_ACQUIRE_TIMEOUT@ (seconds before 'use' gives up).
--
-- Errors are wrapped in 'DBError' so callers don't have to import
-- @hasql-pool@ to handle them. The pool itself is opaque; callers go
-- through 'runSession' to do anything with it.
module Pelotero.DB.Pool
  ( -- * Configuration
    DBConfig(..)
  , defaultDBConfig
  , loadDBConfig
    -- * Pool lifecycle
  , Pool
  , acquire
  , release
  , runSession
  , runTransaction
    -- * Errors
  , DBError(..)
  , renderDBError
  ) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Connection.Setting                  as ConnSetting
import qualified Hasql.Connection.Setting.Connection       as ConnConnection
import qualified Hasql.Connection.Setting.Connection.Param as ConnParam
import           Hasql.Pool (Pool)
import qualified Hasql.Pool        as Pool
import qualified Hasql.Pool.Config as PoolConfig
import           Hasql.Session     (Session)
import System.Environment (lookupEnv)
import Text.Read          (readMaybe)
import qualified Hasql.Transaction          as Tx
import qualified Hasql.Transaction.Sessions as TxS

--------------------------------------------------------------------------------
-- Configuration

data DBConfig = DBConfig
  { dbHost           :: !Text
  , dbPort           :: !Int
  , dbName           :: !Text
  , dbUser           :: !Text
  , dbPassword       :: !Text
  , dbPoolSize       :: !Int
  , dbAcquireTimeout :: !Double  -- ^ seconds
  }
  deriving stock (Show, Eq)

defaultDBConfig :: DBConfig
defaultDBConfig = DBConfig
  { dbHost           = "localhost"
  , dbPort           = 5433
  , dbName           = "pelotero-engine"
  , dbUser           = ""
  , dbPassword       = ""
  , dbPoolSize       = 10
  , dbAcquireTimeout = 10.0
  }

loadDBConfig :: IO DBConfig
loadDBConfig = do
  host       <- envText "PGHOST"     (dbHost           defaultDBConfig)
  port       <- envRead "PGPORT"     (dbPort           defaultDBConfig)
  name       <- envText "PGDATABASE" (dbName           defaultDBConfig)
  user       <- envText "PGUSER"     (dbUser           defaultDBConfig)
  pw         <- envText "PGPASSWORD" (dbPassword       defaultDBConfig)
  poolSize   <- envRead "PELOTERO_DB_POOL_SIZE"       (dbPoolSize       defaultDBConfig)
  acquireSec <- envRead "PELOTERO_DB_ACQUIRE_TIMEOUT" (dbAcquireTimeout defaultDBConfig)
  pure DBConfig
    { dbHost           = host
    , dbPort           = port
    , dbName           = name
    , dbUser           = user
    , dbPassword       = pw
    , dbPoolSize       = poolSize
    , dbAcquireTimeout = acquireSec
    }
  where
    envText :: String -> Text -> IO Text
    envText k def = maybe def T.pack <$> lookupEnv k

    envRead :: Read a => String -> a -> IO a
    envRead k def = do
      m <- lookupEnv k
      pure $ case m >>= readMaybe of
        Just v  -> v
        Nothing -> def

--------------------------------------------------------------------------------
-- Pool lifecycle

poolConfig :: DBConfig -> PoolConfig.Config
poolConfig DBConfig{..} = PoolConfig.settings
  [ PoolConfig.size dbPoolSize
  , PoolConfig.acquisitionTimeout (realToFrac dbAcquireTimeout)
  , PoolConfig.staticConnectionSettings
      [ ConnSetting.connection $ ConnConnection.params
          [ ConnParam.host     dbHost
          , ConnParam.port     (fromIntegral dbPort)
          , ConnParam.dbname   dbName
          , ConnParam.user     dbUser
          , ConnParam.password dbPassword
          ]
      ]
  ]

acquire :: DBConfig -> IO Pool
acquire = Pool.acquire . poolConfig

release :: Pool -> IO ()
release = Pool.release

runSession :: Pool -> Session a -> IO (Either DBError a)
runSession pool sess = do
  res <- Pool.use pool sess
  pure $ case res of
    Right a  -> Right a
    Left err -> Left (PoolUsageError (T.pack (show err)))

runTransaction :: Pool -> Tx.Transaction a -> IO (Either DBError a)
runTransaction pool tx =
  runSession pool (TxS.transaction TxS.ReadCommitted TxS.Write tx)

--------------------------------------------------------------------------------
-- Errors

data DBError
  = PoolUsageError !Text
  | MigrationError !Text
  deriving stock (Show)

instance Exception DBError

renderDBError :: DBError -> Text
renderDBError = \case
  PoolUsageError t -> "DB pool usage error: " <> t
  MigrationError t -> "Migration error: "     <> t