{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson
    ( eitherDecodeFileStrict, eitherDecodeFileStrict, toJSON )
import Data.Aeson.Schema
import qualified Data.Text as T
import InputSchemas (Boxscore, Player)
import Data.HashMap.Strict (fromList, union)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)

main :: IO ()
main = do
  -- Then, load data from a file
  obj <-
    either fail return
      =<< eitherDecodeFileStrict "testFiles/716896_boxscore.json" :: IO (Object Boxscore)
  let
    awayPlayers = [get| obj.teams.away.players[] |]
    homePlayers = [get| obj.teams.home.players[] |]
    allPlayers = awayPlayers ++ homePlayers
  -- print all the users' ids
  print obj

-- flip mapM_ [get| obj.teams.away.players.values |] $ \value -> do
--   -- for each player, print out some information
--   putStrLn $ "Details for user #" ++ show [get| user.id |] ++ ":"
--   putStrLn $ "* Name: " ++ T.unpack [get| user.name |]
--   putStrLn $ "* Age: " ++ maybe "N/A" show [get| user.age |]
--   case [get| user.groups |] of
--     Nothing -> putStrLn "* No groups"
--     Just groups -> putStrLn $ "* Groups: " ++ show groups