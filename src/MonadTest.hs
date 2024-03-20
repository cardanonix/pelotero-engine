module Main where

import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified Ranking as PR
import Validators 
    ( countPlayers
    , findPlayer
    , queryDraftRosterLmt 
    )
import Utility
    ( positionCodeToDraftText
    ,  readJson
    ,  writeJson
    , positionCodeToDraftText
    , createLgManager
    )
import DraftM


initialDraftState :: C.Configuration -> [O.OfficialPlayer] -> [Int] -> DraftState
initialDraftState config players availableIds = DraftState {
    rosters = [],
    currentPick = 0,
    allPlayers = players,
    availableIds = availableIds,
    config = config
}

runDraft :: DraftState -> DraftM a -> IO (Either DraftError a, DraftState)
runDraft initialState action = runStateT (runExceptT action) initialState

main :: IO ()
main = do
  let initialState = DraftState { rosters = [], currentPick = 0 }
  (result, finalState) <- runDraft initialState $ draftCycleMonad [1,2,3] rankings -- replace with actual call
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStrLn "Draft completed successfully"
  -- More logic as needed...