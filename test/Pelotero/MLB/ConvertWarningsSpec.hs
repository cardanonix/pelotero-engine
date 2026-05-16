{-# LANGUAGE OverloadedStrings #-}


module Pelotero.MLB.ConvertWarningsSpec (spec) where

import           Data.Aeson                  (eitherDecodeStrict)
import qualified Data.ByteString             as BS

import           Test.Hspec
                     ( Spec
                     , describe
                     , expectationFailure
                     , it
                     , shouldBe
                     , shouldSatisfy
                     )

import           Pelotero.Domain.Id          (GameId (..))
import           Pelotero.Domain.Stats       (PitchingStats (..))
import           Pelotero.MLB.Convert
                     ( BoxscoreEntry (..)
                     , ConvertWarning (..)
                     , convertBoxscore
                     )
import           Pelotero.MLB.Wire.Boxscore  ()

spec :: Spec
spec = describe "convertBoxscore warning conditions" $ do

  describe "WireFieldDiscrepancy" $ do

    it "fires when inningsPitched and outs disagree" $
      runWarnings disagreeingBoxscore
        `shouldSatisfyW` any isWireFieldDiscrepancy

    it "stays silent when the two fields agree" $
      runWarnings agreeingBoxscore
        `shouldBeW` []

    it "stays silent when only outs is present (no IP string to compare)" $
      runWarnings outsOnlyBoxscore
        `shouldBeW` []

    it "stays silent when only inningsPitched is present" $
      runWarnings ipOnlyBoxscore
        `shouldBeW` []

    it "prefers the parsed IP string over the raw outs field when they disagree" $
      case eitherDecodeStrict disagreeingBoxscore of
        Left err  -> expectationFailure ("decode failed: " <> err)
        Right wbs -> do
          let (_, entries) = convertBoxscore (GameId 0) wbs
              pitchings    = [ ps | e <- entries, Just ps <- [boxPitching e] ]
          case pitchings of
            [ps] -> pitOuts ps `shouldBe` Just 24
            _    -> expectationFailure
                      ( "expected exactly one pitching appearance; got "
                     <> show (length pitchings)
                      )




runWarnings :: BS.ByteString -> IO [ConvertWarning]
runWarnings bs = case eitherDecodeStrict bs of
  Left err  -> expectationFailure ("decode failed: " <> err) >> pure []
  Right wbs -> pure (fst (convertBoxscore (GameId 0) wbs))

shouldSatisfyW :: IO [ConvertWarning] -> ([ConvertWarning] -> Bool) -> IO ()
shouldSatisfyW act p = act >>= \ws -> ws `shouldSatisfy` p

shouldBeW :: IO [ConvertWarning] -> [ConvertWarning] -> IO ()
shouldBeW act expected = act >>= \ws -> ws `shouldBe` expected

isWireFieldDiscrepancy :: ConvertWarning -> Bool
isWireFieldDiscrepancy WireFieldDiscrepancy {} = True
isWireFieldDiscrepancy _                       = False




disagreeingBoxscore :: BS.ByteString
disagreeingBoxscore = pitcherEnvelope
  "\"gamesPlayed\":1,\"gamesStarted\":1,\"battersFaced\":28,\
  \\"inningsPitched\":\"8.0\",\"outs\":23"

agreeingBoxscore :: BS.ByteString
agreeingBoxscore = pitcherEnvelope
  "\"gamesPlayed\":1,\"gamesStarted\":1,\"battersFaced\":28,\
  \\"inningsPitched\":\"8.0\",\"outs\":24"

outsOnlyBoxscore :: BS.ByteString
outsOnlyBoxscore = pitcherEnvelope
  "\"gamesPlayed\":1,\"gamesStarted\":1,\"battersFaced\":28,\"outs\":24"

ipOnlyBoxscore :: BS.ByteString
ipOnlyBoxscore = pitcherEnvelope
  "\"gamesPlayed\":1,\"gamesStarted\":1,\"battersFaced\":28,\
  \\"inningsPitched\":\"8.0\""

pitcherEnvelope :: BS.ByteString -> BS.ByteString
pitcherEnvelope pitchingFields = mconcat
  [ "{\"teams\":{\"away\":{\"players\":{"
  , "\"ID999\":{"
  , "\"person\":{\"id\":999},"
  , "\"parentTeamId\":1,"
  , "\"stats\":{\"pitching\":{"
  , pitchingFields
  , "}}"
  , "}"
  , "}},\"home\":{\"players\":{}}}}"
  ]