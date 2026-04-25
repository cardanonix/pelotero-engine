-- | Wire shape for @\/api\/v1\/game\/{id}\/boxscore@. We map the deeply-
-- nested JSON onto a flatter structure here; conversion to per-player domain
-- 'Pelotero.Domain.Stats' happens in "Pelotero.MLB.Convert".
module Pelotero.MLB.Wire.Boxscore
  ( WireBoxscore(..)
  , WireBoxTeams(..)
  , WireBoxTeam(..)
  , WireBoxPlayer(..)
  , WireBoxPerson(..)
  , WireBoxStats(..)
  , WireBoxBatting(..)
  , WireBoxPitching(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Map.Strict (Map)
import Data.Text (Text)

newtype WireBoxscore = WireBoxscore
  { wbsTeams :: WireBoxTeams }
  deriving stock (Show, Eq)

instance FromJSON WireBoxscore where
  parseJSON = withObject "WireBoxscore" $ \o ->
    WireBoxscore <$> o .: "teams"

data WireBoxTeams = WireBoxTeams
  { wbtAway :: WireBoxTeam
  , wbtHome :: WireBoxTeam
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxTeams where
  parseJSON = withObject "WireBoxTeams" $ \o -> WireBoxTeams
    <$> o .: "away"
    <*> o .: "home"

-- | A team's slice of the box score. The @players@ map is keyed by
-- @"ID<playerId>"@ in the wire format (e.g. "ID660271"). We strip that prefix
-- in the converter.
newtype WireBoxTeam = WireBoxTeam
  { wbtPlayers :: Map Text WireBoxPlayer }
  deriving stock (Show, Eq)

instance FromJSON WireBoxTeam where
  parseJSON = withObject "WireBoxTeam" $ \o ->
    WireBoxTeam <$> o .: "players"

data WireBoxPlayer = WireBoxPlayer
  { wbpPerson       :: WireBoxPerson
  , wbpParentTeamId :: Maybe Int
  , wbpStats        :: Maybe WireBoxStats
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxPlayer where
  parseJSON = withObject "WireBoxPlayer" $ \o -> WireBoxPlayer
    <$> o .:  "person"
    <*> o .:? "parentTeamId"
    <*> o .:? "stats"

data WireBoxPerson = WireBoxPerson
  { wbpPersonId :: Int
  , wbpFullName :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxPerson where
  parseJSON = withObject "WireBoxPerson" $ \o -> WireBoxPerson
    <$> o .:  "id"
    <*> o .:? "fullName"

data WireBoxStats = WireBoxStats
  { wbsBatting  :: Maybe WireBoxBatting
  , wbsPitching :: Maybe WireBoxPitching
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxStats where
  parseJSON = withObject "WireBoxStats" $ \o -> WireBoxStats
    <$> o .:? "batting"
    <*> o .:? "pitching"

-- | Raw batting stats from the boxscore. Every field is 'Maybe Int'; the
-- domain type ('Pelotero.Domain.Stats.BattingStats') has the same shape, so
-- conversion is mostly mechanical.
data WireBoxBatting = WireBoxBatting
  { wbbGamesPlayed          :: Maybe Int
  , wbbPlateAppearances     :: Maybe Int
  , wbbAtBats               :: Maybe Int
  , wbbRuns                 :: Maybe Int
  , wbbHits                 :: Maybe Int
  , wbbDoubles              :: Maybe Int
  , wbbTriples              :: Maybe Int
  , wbbHomeRuns             :: Maybe Int
  , wbbRbi                  :: Maybe Int
  , wbbBaseOnBalls          :: Maybe Int
  , wbbIntentionalWalks     :: Maybe Int
  , wbbStrikeOuts           :: Maybe Int
  , wbbStolenBases          :: Maybe Int
  , wbbCaughtStealing       :: Maybe Int
  , wbbHitByPitch           :: Maybe Int
  , wbbSacBunts             :: Maybe Int
  , wbbSacFlies             :: Maybe Int
  , wbbGroundIntoDoublePlay :: Maybe Int
  , wbbGroundIntoTriplePlay :: Maybe Int
  , wbbLeftOnBase           :: Maybe Int
  , wbbTotalBases           :: Maybe Int
  , wbbFlyOuts              :: Maybe Int
  , wbbGroundOuts           :: Maybe Int
  , wbbCatchersInterference :: Maybe Int
  , wbbPickoffs             :: Maybe Int
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxBatting where
  parseJSON = withObject "WireBoxBatting" $ \o -> WireBoxBatting
    <$> o .:? "gamesPlayed"
    <*> o .:? "plateAppearances"
    <*> o .:? "atBats"
    <*> o .:? "runs"
    <*> o .:? "hits"
    <*> o .:? "doubles"
    <*> o .:? "triples"
    <*> o .:? "homeRuns"
    <*> o .:? "rbi"
    <*> o .:? "baseOnBalls"
    <*> o .:? "intentionalWalks"
    <*> o .:? "strikeOuts"
    <*> o .:? "stolenBases"
    <*> o .:? "caughtStealing"
    <*> o .:? "hitByPitch"
    <*> o .:? "sacBunts"
    <*> o .:? "sacFlies"
    <*> o .:? "groundIntoDoublePlay"
    <*> o .:? "groundIntoTriplePlay"
    <*> o .:? "leftOnBase"
    <*> o .:? "totalBases"
    <*> o .:? "flyOuts"
    <*> o .:? "groundOuts"
    <*> o .:? "catchersInterference"
    <*> o .:? "pickoffs"

data WireBoxPitching = WireBoxPitching
  { wbpGamesPlayed             :: Maybe Int
  , wbpGamesStarted            :: Maybe Int
  , wbpGamesFinished           :: Maybe Int
  , wbpCompleteGames           :: Maybe Int
  , wbpShutouts                :: Maybe Int
  , wbpWins                    :: Maybe Int
  , wbpLosses                  :: Maybe Int
  , wbpSaves                   :: Maybe Int
  , wbpSaveOpportunities       :: Maybe Int
  , wbpHolds                   :: Maybe Int
  , wbpBlownSaves              :: Maybe Int
  , wbpInningsPitched          :: Maybe Text
  , wbpOuts                    :: Maybe Int
  , wbpBattersFaced            :: Maybe Int
  , wbpNumberOfPitches         :: Maybe Int
  , wbpStrikes                 :: Maybe Int
  , wbpBalls                   :: Maybe Int
  , wbpHits                    :: Maybe Int
  , wbpDoubles                 :: Maybe Int
  , wbpTriples                 :: Maybe Int
  , wbpHomeRuns                :: Maybe Int
  , wbpRuns                    :: Maybe Int
  , wbpEarnedRuns              :: Maybe Int
  , wbpStrikeOuts              :: Maybe Int
  , wbpBaseOnBalls             :: Maybe Int
  , wbpIntentionalWalks        :: Maybe Int
  , wbpHitBatsmen              :: Maybe Int
  , wbpWildPitches             :: Maybe Int
  , wbpBalks                   :: Maybe Int
  , wbpPickoffs                :: Maybe Int
  , wbpFlyOuts                 :: Maybe Int
  , wbpGroundOuts              :: Maybe Int
  , wbpAirOuts                 :: Maybe Int
  , wbpInheritedRunners        :: Maybe Int
  , wbpInheritedRunnersScored  :: Maybe Int
  , wbpStolenBases             :: Maybe Int
  , wbpCaughtStealing          :: Maybe Int
  , wbpAtBats                  :: Maybe Int
  , wbpRbi                     :: Maybe Int
  , wbpSacBunts                :: Maybe Int
  , wbpSacFlies                :: Maybe Int
  , wbpCatchersInterference    :: Maybe Int
  , wbpPassedBall              :: Maybe Int
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxPitching where
  parseJSON = withObject "WireBoxPitching" $ \o -> WireBoxPitching
    <$> o .:? "gamesPlayed"
    <*> o .:? "gamesStarted"
    <*> o .:? "gamesFinished"
    <*> o .:? "completeGames"
    <*> o .:? "shutouts"
    <*> o .:? "wins"
    <*> o .:? "losses"
    <*> o .:? "saves"
    <*> o .:? "saveOpportunities"
    <*> o .:? "holds"
    <*> o .:? "blownSaves"
    <*> o .:? "inningsPitched"
    <*> o .:? "outs"
    <*> o .:? "battersFaced"
    <*> o .:? "numberOfPitches"
    <*> o .:? "strikes"
    <*> o .:? "balls"
    <*> o .:? "hits"
    <*> o .:? "doubles"
    <*> o .:? "triples"
    <*> o .:? "homeRuns"
    <*> o .:? "runs"
    <*> o .:? "earnedRuns"
    <*> o .:? "strikeOuts"
    <*> o .:? "baseOnBalls"
    <*> o .:? "intentionalWalks"
    <*> o .:? "hitBatsmen"
    <*> o .:? "wildPitches"
    <*> o .:? "balks"
    <*> o .:? "pickoffs"
    <*> o .:? "flyOuts"
    <*> o .:? "groundOuts"
    <*> o .:? "airOuts"
    <*> o .:? "inheritedRunners"
    <*> o .:? "inheritedRunnersScored"
    <*> o .:? "stolenBases"
    <*> o .:? "caughtStealing"
    <*> o .:? "atBats"
    <*> o .:? "rbi"
    <*> o .:? "sacBunts"
    <*> o .:? "sacFlies"
    <*> o .:? "catchersInterference"
    <*> o .:? "passedBall"