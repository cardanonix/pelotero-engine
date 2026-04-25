-- | Entry point for integration tests. Each spec is rollback-isolated:
-- it opens its own session, runs work inside a transaction that's
-- explicitly aborted at the end, so no test commits anything to the DB.
--
-- These tests REQUIRE a reachable Postgres on the standard libpq env vars
-- and will FAIL (not skip) if one isn't available. That's intentional —
-- silent skips are how integration tests rot.
module Main (main) where

import Test.Hspec (hspec)

import qualified IntegrationTest.TeamRepoSpec   as TeamRepoSpec
import qualified IntegrationTest.PlayerRepoSpec as PlayerRepoSpec

main :: IO ()
main = hspec $ do
  TeamRepoSpec.spec
  PlayerRepoSpec.spec