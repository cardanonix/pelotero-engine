module Main (main) where

import qualified Data.Text.IO as TIO
import qualified Pelotero.Prelude as P

main :: IO ()
main = do
  TIO.putStrLn (P.appName <> " " <> P.appVersion)
  TIO.putStrLn "Phase 0 skeleton. Subcommands TBD."