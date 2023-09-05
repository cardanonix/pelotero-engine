{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import qualified Data.Text as T
import InputSchemas


main :: IO ()
main = do
  -- Then, load data from a file
  obj <- either fail return =<<
    eitherDecodeFileStrict "testFiles/testfile.json" :: IO (Object Boxscore)

  -- print all the users' ids
  print [get| obj.teams.away.players.values.allPositions[] |]


  -- flip mapM_ [get| obj.teams.away.players.values |] $ \value -> do
  --   -- for each player, print out some information
  --   putStrLn $ "Details for user #" ++ show [get| user.id |] ++ ":"
  --   putStrLn $ "* Name: " ++ T.unpack [get| user.name |]
  --   putStrLn $ "* Age: " ++ maybe "N/A" show [get| user.age |]
  --   case [get| user.groups |] of
  --     Nothing -> putStrLn "* No groups"
  --     Just groups -> putStrLn $ "* Groups: " ++ show groups