module Main where

-- import Control.Monad (forM, foldM)
-- import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as T
-- import GHC.Generics (Generic)
-- import Data.Time.Clock (UTCTime, getCurrentTime)
-- import Data.Time.Format (formatTime, defaultTimeLocale)

-- import Data.Maybe (mapMaybe, fromMaybe)
-- import Data.List
--     ( find,
--       delete,
--       sortOn,
--       sortBy,
--       findIndex,
--       sortOn,
--       findIndex,
--       sortOn,
--       findIndex,
--       sortOn,
--       findIndex )

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

-- import Draft
import DraftMonad


runDraft :: DraftState -> DraftMonad a -> IO (Either DraftError a, DraftState)
runDraft initialState action = runStateT (runExceptT action) initialState

main :: IO ()
main = do
  let initialState = DraftState { rosters = [], currentPick = 0 }
  (result, finalState) <- runDraft initialState $ draftCycleMonad [1,2,3] rankings -- replace with actual call
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStrLn "Draft completed successfully"
  -- More logic as needed...