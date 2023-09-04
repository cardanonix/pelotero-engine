{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (eitherDecodeFileStrict, toJSON)
import Data.Aeson.Schema
import Data.HashMap.Strict (union, fromList)
import qualified Data.Text as T

flattenGameData :: Object GameStats -> Int -> HashMap Text Object
flattenGameData gameStats gameId =
  let
    awayPlayers = [get| gameStats.teams.away.players |]
    homePlayers = [get| gameStats.teams.home.players |]
    allPlayers = awayPlayers ++ homePlayers

    playerData player = 
      let
        playerId = T.pack . show $ [get| player.person.id |]
        playerObj = [schemaObj|
          {
            id: player.person.id,
            fullName: player.person.fullName,
            stats: {
              ($gameId): {
                parentTeamId: player.parentTeamId,
                allPositions: Maybe (map (\position -> read (T.unpack [get| position.code |]) :: Int) [get| player.allPositions |]),
                status: player.status.code,
                batting: Maybe (let
                            battingStats = [get| player.stats.batting |]
                            in battingStats { 
                                note = Nothing, 
                                summary = Nothing, 
                                stolenBasePercentage = Nothing, 
                                atBatsPerHomeRun = Nothing
                            }),
                pitching: Maybe (let
                            pitchingStats = [get| player.stats.pitching |]
                            in pitchingStats {
                                note = Nothing,
                                summary = Nothing,
                                stolenBasePercentage = Nothing,
                                strikePercentage = Nothing,
                                homeRunsPer9 = Nothing,
                                runsScoredPer9 = Nothing
                            })
              }
            }
          }
        |]
      in (playerId, playerObj)

    extractedData = map playerData (filter (\player -> [get| player.allPositions |] /= Nothing) allPlayers)

  in fromList extractedData