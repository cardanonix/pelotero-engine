module Main (main) where

import           Control.Monad           (foldM)
import qualified Data.ByteString         as BS
import qualified Pelotero.MLB.Fetch      as Fetch
import           System.Directory        (createDirectoryIfMissing, doesFileExist, getFileSize)
import           System.Exit             (ExitCode(..), exitWith)
import           System.FilePath         (takeDirectory)
import           System.IO               (hPutStrLn, stderr)

-- The fixture manifest. Add or remove gamePks here as the test suite evolves.
-- Each entry pins one MLB Stats API boxscore JSON to a local path. The files
-- are not committed (see .gitignore); CI fetches them on demand via
-- "nix develop --command fetch-fixtures".
fixtures :: [(Int, FilePath)]
fixtures =
  [ (823385, "test/fixtures/boxscore-823385.json")
  , (824764, "test/fixtures/boxscore-824764.json")
  ]

main :: IO ()
main = do
  ok <- foldM step True fixtures
  if ok
    then putStrLn "All fixtures present."
    else exitWith (ExitFailure 1)
  where
    step acc spec = do
      this <- fetchOne spec
      pure (acc && this)

fetchOne :: (Int, FilePath) -> IO Bool
fetchOne (pk, path) = do
  present <- alreadyPresent path
  if present
    then do
      putStrLn ("skip   " <> path)
      pure True
    else do
      putStrLn ("fetch  " <> path)
      result <- Fetch.fetchBoxscoreRaw pk
      case result of
        Left err -> do
          hPutStrLn stderr ("ERROR  " <> path <> ": " <> err)
          pure False
        Right bs -> do
          createDirectoryIfMissing True (takeDirectory path)
          BS.writeFile path bs
          pure True

alreadyPresent :: FilePath -> IO Bool
alreadyPresent path = do
  exists <- doesFileExist path
  if not exists
    then pure False
    else do
      size <- getFileSize path
      pure (size > 0)