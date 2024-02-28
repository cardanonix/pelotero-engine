module Main (main) where

import Config as C
import Validators
import Utility

main :: IO ()
main = do
    parsedConfig <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
    let fileNames =
            [ "testFiles/prototype_config/valid_roster.json"
            , "testFiles/prototype_config/invalid_roster.json"
            , "testFiles/prototype_config/invalid_lineup.json"
            , "testFiles/appData/rosters/team_001.json"
            , "testFiles/appData/rosters/team_002.json"
            , "testFiles/appData/rosters/team_003.json" -- <-- too many outfielders
            , "testFiles/appData/rosters/team_004.json" -- <-- too many outfielders
            ]
    filesContent <- mapM (\path -> readJson path :: IO FileContent) fileNames

    case parsedConfig of
        Left err -> putStrLn $ "Failed to parse Config JSON: " ++ err
        Right config -> processConfigResults config (zip fileNames filesContent)
