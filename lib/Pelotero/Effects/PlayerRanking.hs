{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Pelotero.Effects.PlayerRanking
  ( PlayerRanking(..)
  , getRankingsForTeam
  , replaceRankings
  , clearRankings
  , getRankingCount
  , runPlayerRankingDB
  ) where

import Data.Int (Int64)

import Effectful (Effect, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import qualified Pelotero.DB.PlayerRanking as PRRepo
import           Pelotero.DB.PlayerRanking (PlayerRankingRow)
import           Pelotero.Domain.Id        (DbLeagueTeamId)
import           Pelotero.Effects.Database (Database, runTx)

data PlayerRanking :: Effect where
  GetRankingsForTeam :: DbLeagueTeamId                          -> PlayerRanking m [PlayerRankingRow]
  ReplaceRankings    :: DbLeagueTeamId -> [PlayerRankingRow]    -> PlayerRanking m ()
  ClearRankings      :: DbLeagueTeamId                          -> PlayerRanking m ()
  GetRankingCount    :: DbLeagueTeamId                          -> PlayerRanking m Int64

type instance DispatchOf PlayerRanking = 'Dynamic

getRankingsForTeam
  :: PlayerRanking E.:> es
  => DbLeagueTeamId -> E.Eff es [PlayerRankingRow]
getRankingsForTeam = send . GetRankingsForTeam

replaceRankings
  :: PlayerRanking E.:> es
  => DbLeagueTeamId -> [PlayerRankingRow] -> E.Eff es ()
replaceRankings ltid rows = send (ReplaceRankings ltid rows)

clearRankings :: PlayerRanking E.:> es => DbLeagueTeamId -> E.Eff es ()
clearRankings = send . ClearRankings

getRankingCount
  :: PlayerRanking E.:> es
  => DbLeagueTeamId -> E.Eff es Int64
getRankingCount = send . GetRankingCount

runPlayerRankingDB
  :: Database E.:> es
  => E.Eff (PlayerRanking : es) a
  -> E.Eff es a
runPlayerRankingDB = interpret_ $ \case
  GetRankingsForTeam ltid     -> runTx (PRRepo.getRankingsForTeamT ltid)
  ReplaceRankings ltid rows   -> runTx (PRRepo.replaceRankingsT ltid rows)
  ClearRankings ltid          -> runTx (PRRepo.clearRankingsT ltid)
  GetRankingCount ltid        -> runTx (PRRepo.getRankingCountT ltid)