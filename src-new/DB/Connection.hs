{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB.Connection
  ( DBConfig(..)
  , initializeDB
  , withConnection
  , defaultConfig
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO)
import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getLoginName)

data DBConfig = DBConfig
  { dbHost     :: String
  , dbPort     :: Int
  , dbName     :: String
  , dbUser     :: String
  , dbPassword :: String
  , poolSize   :: Int
  }

defaultConfig :: IO DBConfig
defaultConfig = do
  user <- getLoginName
  pure DBConfig
    { dbHost     = "localhost"
    , dbPort     = 5432
    , dbName     = "fantasy_league"
    , dbUser     = user
    , dbPassword = "postgres"
    , poolSize   = 5
    }

initializeDB :: DBConfig -> IO (Pool.Pool Connection)
initializeDB config = do
  let poolConfig =
        Pool.defaultPoolConfig
          (connectWithRetry config)
          close
          0.5
          (fromIntegral $ poolSize config)
  pool <- Pool.newPool poolConfig
  Pool.withResource pool $ \conn -> do
    _ <- query_ conn "SELECT 1" :: IO [Only Int]
    pure ()
  pure pool

connectWithRetry :: DBConfig -> IO Connection
connectWithRetry DBConfig{..} = go 5
  where
    go :: Int -> IO Connection
    go 0 = error "Failed to connect to database after 5 attempts"
    go n = do
      let connInfo = defaultConnectInfo
            { connectHost     = dbHost
            , connectPort     = fromIntegral dbPort
            , connectDatabase = dbName
            , connectUser     = dbUser
            , connectPassword = dbPassword
            }
      catch (connect connInfo) $ \e -> do
        hPutStrLn stderr $ "Connection failed (" ++ show n ++ " retries left): " ++ show (e :: SqlError)
        threadDelay 2000000
        go (n - 1)

withConnection :: Pool.Pool Connection -> (Connection -> IO a) -> IO a
withConnection = Pool.withResource