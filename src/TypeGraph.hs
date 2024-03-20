{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy ( Text, pack )
import qualified System.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.GraphViz.Printing (renderDot)

main :: IO ()
main = do
    writeConfigurationGraph
    TIO.putStrLn "Configuration graph written to testFiles/configurationGraph.dot"


writeConfigurationGraph :: IO ()
writeConfigurationGraph = do
    let dotGraph = digraph (Str "ConfigurationGraph") $ do
          -- Nodes with Labels
          node (pack "Configuration") [Label $ StrLabel "Configuration\nstatus, leagueID, commissioner, lgMembers"]
          node (pack "PointParameters") [Label $ StrLabel "PointParameters\nlg_style, start_UTC, end_UTC"]
          node (pack "BattingMults") [Label $ StrLabel "BattingMults\nlgb_single, lgb_double, ..."]
          node (pack "PitchingMults") [Label $ StrLabel "PitchingMults\nlgp_win, lgp_save, ..."]
          node (pack "LgRosterLmts") [Label $ StrLabel "LgRosterLmts\nlg_catcher, lg_first, ..."]
          node (pack "DraftParameters") [Label $ StrLabel "DraftParameters\nautoDraft, autoDraft_UTC"]
          node (pack "DraftRosterLmts") [Label $ StrLabel "DraftRosterLmts\ndr_catcher, dr_first, ..."]

          -- Edges
          edge (pack "Configuration") (pack "PointParameters") []
          edge (pack "Configuration") (pack "DraftParameters") []
          edge (pack "PointParameters") (pack "BattingMults") []
          edge (pack "PointParameters") (pack "PitchingMults") []
          edge (pack "PointParameters") (pack "LgRosterLmts") []
          edge (pack "DraftParameters") (pack "DraftRosterLmts") []

    writeFile "testFiles/configurationGraph.dot" (TL.unpack (renderDot $ toDot dotGraph))
    
writeInputGraph :: IO ()
writeInputGraph = do
    let dotGraph = digraph (Str "ConfigurationGraph") $ do
          -- Nodes with explicit type for string literals
          node (pack "Configuration") []
          node (pack "PointParameters") []
          node (pack "BattingMults") []
          node (pack "PitchingMults") []
          node (pack "LgRosterLmts") []
          node (pack "DraftParameters") []
          node (pack "DraftRosterLmts") []

          -- Edges with explicit type for string literals
          edge (pack "Configuration") (pack "PointParameters") []
          edge (pack "Configuration") (pack "DraftParameters") []
          edge (pack "PointParameters") (pack "BattingMults") []
          edge (pack "PointParameters") (pack "PitchingMults") []
          edge (pack "PointParameters") (pack "LgRosterLmts") []
          edge (pack "DraftParameters") (pack "DraftRosterLmts") []

    -- Convert Text to String and then write using Prelude.writeFile
    writeFile "configurationGraph.dot" (TL.unpack (renderDot $ toDot dotGraph))
