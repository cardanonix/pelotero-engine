module Pelotero.MatchupSpec (spec) where

import           Hedgehog                (forAll, (===))
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Test.Hspec.Hedgehog     (hedgehog)

import           Pelotero.Domain.Id      (DbLeagueTeamId (..))
import           Pelotero.Domain.Scoring (Points (..))
import           Pelotero.Matchup
import           Pelotero.Score          (TeamScore (..))

spec :: Spec
spec = describe "Pelotero.Matchup" $ do

  it "the higher total wins and equal totals tie" $ do
    matchupOutcome (decideMatchup (team 1 10) (team 2 7))  `shouldBe` HomeWins
    matchupOutcome (decideMatchup (team 1 7)  (team 2 10)) `shouldBe` AwayWins
    matchupOutcome (decideMatchup (team 1 7)  (team 2 7))  `shouldBe` Tied

  it "compares negative totals correctly" $
    matchupOutcome (decideMatchup (team 1 (-3)) (team 2 (-5))) `shouldBe` HomeWins

  it "matchupWinner names the winning side, or nobody for a tie" $ do
    fmap tsTeam (matchupWinner (decideMatchup (team 1 10) (team 2 7)))
      `shouldBe` Just (DbLeagueTeamId 1)
    fmap tsTeam (matchupWinner (decideMatchup (team 1 1) (team 2 7)))
      `shouldBe` Just (DbLeagueTeamId 2)
    matchupWinner (decideMatchup (team 1 7) (team 2 7)) `shouldBe` Nothing

  it "swapping the sides swaps the outcome" $
    hedgehog $ do
      a <- forAll (Gen.integral (Range.linear (-50) 50))
      b <- forAll (Gen.integral (Range.linear (-50) 50))
      let flipped = \case
            HomeWins -> AwayWins
            AwayWins -> HomeWins
            Tied     -> Tied
      matchupOutcome (decideMatchup (team 2 b) (team 1 a))
        === flipped (matchupOutcome (decideMatchup (team 1 a) (team 2 b)))

  it "pairOff uses every element exactly once" $
    hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 21) (Gen.int (Range.linear 0 100)))
      let (pairs, bye) = pairOff xs
      concatMap (\(a, b) -> [a, b]) pairs <> maybe [] pure bye === xs
      (bye /= Nothing) === odd (length xs)

  it "matchupsFor pairs teams in order and reports the bye" $ do
    let (ms, bye) = matchupsFor [team 1 5, team 2 3, team 3 9]
    map (\m -> (tsTeam (matchupHome m), tsTeam (matchupAway m))) ms
      `shouldBe` [(DbLeagueTeamId 1, DbLeagueTeamId 2)]
    fmap tsTeam bye `shouldBe` Just (DbLeagueTeamId 3)

team :: Integer -> Integer -> TeamScore
team tid pts = TeamScore
  { tsTeam        = DbLeagueTeamId (fromIntegral tid)
  , tsPlayers     = []
  , tsTotalPoints = Points (fromIntegral pts)
  }
