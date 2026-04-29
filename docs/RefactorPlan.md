The honest read: the old code is what happens when a project grows by accretion. The new code is a step in the right direction, but it's not consistent with the stack you're using on Cheeblr, and that's the single most important thing to fix before you write more.

## Major problems in the old code

1. **Type duplication is the central disease.** `BattingStats`/`PitchingStats` exist in `Input.hs`, `Stats.hs`, and `Input_trace.hs`, plus near-identical `BattingTotals`/`PitchingTotals` in `Leaderboard.hs`, plus the points-result variants in `Points.hs`. At least four places describe "what does a batter do in a game."

2. **Data flows through a pipeline of slightly-different shapes.** `Input.Player` → `Middle.JsonPlayerData` → `Points.GmPoints`, each stage with its own JSON schema. The literal module name `Middle` is the smell — that's mutation that should be queries against a database.

3. **9-field positional records (`cR/b1R/b2R/.../rpR`)** for both `Roster` and `CurrentLineup`. Every operation that touches a position is a 9-arm case expression. `Map Position [PlayerId]` collapses it all.

4. **`PlayerID` is inconsistent.** Newtype `Int` in `OfficialRoster`, `Text` in `Roster` fields (you serialize IDs as text in lineups!), bare `Text`/`Int` elsewhere. There's also a `PlayerIDstring` lurking. Pick one representation.

5. **`{-# LANGUAGE GADTs #-}` enabled on every module that defines plain ADTs.** Nothing is actually using GADT features. That's a tell that pragma blocks were copy-pasted module-to-module.

6. **`-Wno-deferred-out-of-scope-variables`** in multiple modules. That's the compiler telling you something is broken and you've muted it.

7. **Each executable has its own `main` and there's no library.** Cabal rebuilds shared modules per-executable target. Move it all into a library; thin `app/*.hs` Mains only.

8. **JSON files as the persistence layer.** `appData/rosters/*.json`, `appData/stats/*.json`, `appData/rankings/*.json`, draft results round-tripping back to JSON. You're using the filesystem as a poorly-typed K/V store. This is what you're refactoring away — good.

9. **`Position` is `Text`** with informal codes ("1B"/"first"/"pitcher"). Three translation functions (`positionTextToOfficialCode`, `positionCodeToText`, `positionCodeToOfficialText`, `positionCodeToDraftText`) each do something subtly different. Sum type, parse-don't-validate at the boundary.

10. **`Draft.draftCycle` returns `(state, Maybe String)` and detects failure with `newState == state`.** A state machine running on referential equality of records. Brittle. `Either DraftError DraftState`.

11. **Errors are a mix of `Either String _` and `Either Text _`.** No typed error hierarchy.

12. **No tests.** `Test.hs` reads two JSON files and prints them. That's not a test.

## Issues with the in-progress refactor in `src-new/`

You've made the right move (database) but you're not using the stack you use everywhere else:

1. **`postgresql-simple` instead of `rel8` + `hasql` + `hasql-pool`.** You spent significant effort on Cheeblr migrating *away* from `postgresql-simple` to `rel8`/`hasql`. Why reintroduce it here? You'll just migrate it again. **This is the single biggest correction I'd make before you write another line of DB code.**

2. **No effects layer.** Cheeblr uses `effectful`. This is plain IO with hidden capabilities. If you want this to ever be testable in-memory (and you should), the same effect discipline needs to apply.

3. **No Katip.** `hPutStrLn stderr` is fine for a script; not for a system you want to keep.

4. **`getLoginName` as the DB user default and a hardcoded `"postgres"` password.** This needs to come from `sops-nix` the same way Cheeblr does it.

5. **Migrations are inline `execute_` with no version table.** Add a `schema_migrations` table or use `hasql-migration`/`dbmate`. You'll regret unversioned migrations the first time you change a column type.

6. **`fromMaybe "" mlbUseName` etc.** You're conflating "absent" and "empty string." Either model absence with `Maybe` end-to-end, or assert presence at the parse boundary and store `NOT NULL`. Don't paper over it with `""`.

7. **`upsertPlayers` does N round-trips inside a transaction.** `executeMany` or generate a multi-row `INSERT ... ON CONFLICT`. ~1500 active players makes it a non-issue today, but it's a habit worth getting right.

8. **`error "Failed to connect..."`** in `connectWithRetry`. Throw a typed exception or return `Either`. Bare `error` in production code is sloppy.

9. **No `flake.nix`.** You prefer flakes everywhere else; this project should match.

## What I'd actually do

Match Cheeblr's stack. The duplication of mental models across your projects is the real cost — every time you switch projects you pay a tax remembering which DB library, which logging library, which effect approach.

**Target stack:**
- `rel8` + `hasql` + `hasql-pool` + `hasql-migration`
- `effectful` for the effect layer
- `katip` with the broadcast-scribe pattern from Cheeblr
- `aeson` only at the wire boundary
- `crem` for the draft state machine (textbook use case — finite states, well-defined transitions)
- `hedgehog` for property tests on point calc and draft invariants
- `sops-nix` for DB credentials
- Flake with a `default` dev shell and a minimal `.#ci` shell, mirroring Cheeblr

**Phased plan:**

**Phase 0 — Skeleton.** Library + `app/*.hs` mains. `flake.nix` with dev/CI shells. `sops-nix` for the DB password. Local dev DB launched from the flake.

**Phase 1 — Single source of truth for domain types.**
- `Pelotero.Domain.Player`, `.Team`, `.Position`, `.Stats` (one `Batting`, one `Pitching`, that's it)
- `Position` is a sum type with `parsePositionCode` / `renderPositionCode`
- Newtypes: `PlayerId`, `TeamId`, `GameId`, `SeasonYear`, all `Int` underneath
- Wire types live in `Pelotero.MLB.Wire.*` and convert to domain at the boundary. Domain types never appear in `aeson` instances.

**Phase 2 — Schema + rel8.**
- Tables: `player`, `team`, `game`, `game_player_batting`, `game_player_pitching`, `league_config`, `league_team`, `roster_slot`, `lineup_slot`, `player_ranking`, `draft_pick`
- `roster_slot` / `lineup_slot` shaped as `(team_id, position, player_id)` — kills the 9-field record problem at the storage layer
- Migrations versioned in `db/migrations/*.sql`, applied at startup
- rel8 schema definitions in `Pelotero.DB.Schema.*`. Write the SQL first; let the types follow the schema, not the other way around.

**Phase 3 — Effects.**
- `DB :: Effect` (`getPlayer`, `upsertPlayers`, `getRoster`, `recordDraftPick`, …)
- `MLBClient :: Effect` (`fetchActiveRoster`, `fetchSchedule`, `fetchBoxscore`)
- `Logging :: Effect` (Katip-backed)
- `Time :: Effect` (so time-sensitive logic is testable)
- Production interpreters real; test interpreters in-memory.

**Phase 4 — Port the scraper.** `Scraper.hs` becomes `Pelotero.MLB.Sync` calling the `MLBClient` and `DB` effects. Drop filesystem writes entirely. Add idempotency: re-running `sync-rosters 2024` is a no-op when the upstream checksum hasn't changed (you've already got the checksum concept — actually use it).

**Phase 5 — Port domain logic.**
- `Pelotero.Points.Calculate` — `calcBattingPoints`/`calcPitchingPoints` are already mostly pure; port straight across with hedgehog tests.
- `Pelotero.Draft` as a `crem` state machine. `States = WaitingToStart | Drafting | Complete`, transitions `StartDraft | PickPlayer | EndDraft`. The `(state, Maybe String)` pattern dies.
- `Pelotero.Validators` — typed `ValidationError`, not `String`.

**Phase 6 — Apps.**
```
pelotero sync rosters --season 2024
pelotero sync stats --from 2024-04-01 --to 2024-04-07
pelotero league validate --league-id <id>
pelotero draft run --league-id <id>
pelotero h2h <team-a> <team-b> --from <date> --to <date>
```

**Phase 7 — Delete.** Once each old executable has a replacement passing acceptance tests, delete `src/`, `src/ADT/`, `src/League/`, the `appData/` JSON files, and the old executables from the cabal file. Do not leave both halves around — that's how the current mess started.
