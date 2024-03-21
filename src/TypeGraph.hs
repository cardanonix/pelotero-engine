{-# LANGUAGE OverloadedStrings #-}

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as TLIO
import System.Process (callCommand)

main :: IO ()
main = do
    writeConfigurationGraph
    putStrLn "Configuration graph written to configurationGraph.dot"
    -- Convert .dot to .png using system call
    let dotCommand = "dot -Tpng configurationGraph.dot -o configurationGraph.png"
    callCommand dotCommand
    putStrLn "Configuration graph image written to configurationGraph.png"

-- writeConfigurationGraph :: IO ()
-- writeConfigurationGraph = do
--     let dotGraph = digraph (Str "ConfigurationGraph") $ do
--           -- Nodes with Labels
--           node (pack "Configuration") [Label $ StrLabel "Configuration\nstatus, leagueID, commissioner, lgMembers"]
--           node (pack "PointParameters") [Label $ StrLabel "PointParameters\nlg_style, start_UTC, end_UTC"]
--           node (pack "BattingMults") [Label $ StrLabel "BattingMults\nlgb_single, lgb_double, ..."]
--           node (pack "PitchingMults") [Label $ StrLabel "PitchingMults\nlgp_win, lgp_save, ..."]
--           node (pack "LgLineupLmts") [Label $ StrLabel "LgLineupLmts\nlg_catcher, lg_first, ..."]
--           node (pack "DraftParameters") [Label $ StrLabel "DraftParameters\nautoDraft, autoDraft_UTC"]
--           node (pack "DraftRosterLmts") [Label $ StrLabel "DraftRosterLmts\ndr_catcher, dr_first, ..."]

--           -- Edges
--           edge (pack "Configuration") (pack "PointParameters") []
--           edge (pack "Configuration") (pack "DraftParameters") []
--           edge (pack "PointParameters") (pack "BattingMults") []
--           edge (pack "PointParameters") (pack "PitchingMults") []
--           edge (pack "PointParameters") (pack "LgLineupLmts") []
--           edge (pack "DraftParameters") (pack "DraftRosterLmts") []

--     TLIO.writeFile "configurationGraph.dot" (printDotGraph dotGraph)

writeConfigurationGraph :: IO ()
writeConfigurationGraph = do
    let dotGraph = digraph (Str "ConfigurationGraph") $ do
          node (pack "Configuration") [Label $ StrLabel "Configuration\nstatus, leagueID, commissioner, lgMembers"]
          node (pack "PointParameters") [Label $ StrLabel "PointParameters\nlg_style, start_UTC, end_UTC, lg_battingMults, lg_pitchingMults, valid_roster"]
          node (pack "DraftParameters") [Label $ StrLabel "DraftParameters\nautoDraft, autoDraft_UTC, draft_limits"]
          node (pack "BattingMults") [Label $ StrLabel "BattingMults\nlgb_single, lgb_double, lgb_triple, lgb_homerun, lgb_rbi, lgb_run, lgb_base_on_balls, lgb_stolen_base, lgb_hit_by_pitch, lgb_strikeout, lgb_caught_stealing"]
          node (pack "PitchingMults") [Label $ StrLabel "PitchingMults\nlgp_win, lgp_save, lgp_quality_start, lgp_inning_pitched, lgp_strikeout, lgp_complete_game, lgp_shutout, lgp_base_on_balls, lgp_hits_allowed, lgp_earned_runs, lgp_hit_batsman, lgp_loss"]
          node (pack "LgLineupLmts") [Label $ StrLabel "LgLineupLmts\nlg_catcher, lg_first, lg_second, lg_third, lg_shortstop, lg_outfield, lg_utility, lg_s_pitcher, lg_r_pitcher, lg_max_size"]
          node (pack "DraftRosterLmts") [Label $ StrLabel "DraftRosterLmts\ndr_catcher, dr_first, dr_second, dr_third, dr_shortstop, dr_outfield, dr_utility, dr_s_pitcher, dr_r_pitcher"]

          edge (pack "Configuration") (pack "PointParameters") []
          edge (pack "Configuration") (pack "DraftParameters") []
          edge (pack "PointParameters") (pack "BattingMults") []
          edge (pack "PointParameters") (pack "PitchingMults") []
          edge (pack "PointParameters") (pack "LgLineupLmts") []
          edge (pack "DraftParameters") (pack "DraftRosterLmts") []

    TLIO.writeFile "configurationGraph.dot" (printDotGraph dotGraph)