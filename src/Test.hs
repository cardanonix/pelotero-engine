{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import qualified Data.Text as T

-- First, define the schema of the JSON data
type MySchema = [schema|
  {
    users: List {
      id: Int,
      name: Text,
      age: Maybe Int,
      enabled: Bool,
      groups: Maybe List {
        id: Int,
        name: Text,
      },
    },
  }
|]

main :: IO ()
main = do
  -- Then, load data from a file
  obj <- either fail return =<<
    eitherDecodeFileStrict "testFiles/testfile.json" :: IO (Object MySchema)

  -- print all the users' ids
  print [get| obj.users[].id |]

  flip mapM_ [get| obj.users |] $ \user -> do
    -- for each user, print out some information
    putStrLn $ "Details for user #" ++ show [get| user.id |] ++ ":"
    putStrLn $ "* Name: " ++ T.unpack [get| user.name |]
    putStrLn $ "* Age: " ++ maybe "N/A" show [get| user.age |]
    case [get| user.groups |] of
      Nothing -> putStrLn "* No groups"
      Just groups -> putStrLn $ "* Groups: " ++ show groups