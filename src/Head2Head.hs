module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import PointCalc
import Validators
import qualified Config as C
import qualified Roster as R
import qualified Middle as M
import Data.Either (isRight)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Testing the Point Calculation Module:"
    testSuite

testSuite :: IO ()
testSuite = do
    config <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
    roster <- readJson "testFiles/appData/rosters/team_002.json" :: IO (Either String R.LgManager)
    playerData <- readJson "appData/stats/2023_09_30.json" :: IO (Either String [M.JsonPlayerData])
    
    case (config, roster, playerData) of
        (Right c, Right r, Right pd) -> do
            let result = calculateAllPoints c r pd
            printResult result
        _ -> putStrLn "Failed to read data."

printResult :: [(Text, Either Text Double)] -> IO ()
printResult [] = return ()
printResult ((playerId, result):xs) = do
    putStr $ T.unpack playerId ++ ": "
    case result of
        Left error -> putStrLn $ "Error: " ++ T.unpack error
        Right points -> putStrLn $ "Points: " ++ show points
    printResult xs
