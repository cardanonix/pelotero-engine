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

---

Two questions before we start writing code:

1. Are you willing to swap `postgresql-simple` for `rel8` + `hasql` in `src-new/` before adding more tables? It's cheap now (one table, one repo module), expensive later.
2. Do you want this project's flake to match Cheeblr's `haskell.nix`-based setup, or a lighter `nixpkgs.haskellPackages` flake since this is single-target?

Once those are settled I'd start with Phase 0 and Phase 1 — flake + library skeleton + the consolidated domain types — and we can verify the shape before touching the DB.


__________

new notes:
```
The compile error is one specific thing: a misplaced file. Cabal is looking for `lib/Pelotero/DB/RosterSlot.hs` (because the module declares `module Pelotero.DB.RosterSlot`), but the file is sitting at `lib/Pelotero/Domain/RosterSlot.hs`.

```
mv lib/Pelotero/Domain/RosterSlot.hs lib/Pelotero/DB/RosterSlot.hs
```

The file's content is correct — it's a DB-layer module (it imports `Pelotero.DB.Pool`, `Pelotero.DB.Statement`, defines `RosterSlotRow`, runs `Tx.Transaction`), it just got saved to the wrong directory at some point. Once moved, that error clears.

## Other things you'll hit immediately after

**`app/Combined.hs` has three `module Main` declarations stacked in one file.** Either your dumper concatenated DbCheck/Main/FetchRoster into it by mistake, or you've actually got a half-finished merge file in your tree. If it's real, delete it — it'll fail to compile the moment Cabal looks at it. The three real apps (`DbCheck.hs`, `Main.hs`, `FetchRoster.hs`) are fine on their own.

**`error` in pure-ish code that should be `fail`-in-Parser or proper `Either`.**
- `Pelotero.DB.Statement.decJsonb` calls `error` on a bad JSONB row. That's a runtime crash on dirty data inside a `D.Row`. It should `fail` (decoders support that via the underlying parser monad — or just store the raw bytes and parse separately).
- `Pelotero.Domain.Roster`'s `FromJSON RosterLimits`/`LineupLimits` calls `error ("bad slot: ...")` inside a `Parser`. Use `fail` instead. Right now a malformed config file crashes the process instead of returning a JSON parse error.

## Bigger issues worth correcting before you write more tables

**You said Phase 2 is rel8, but there's no rel8 anywhere.** Every repository module is hand-rolled hasql with manual `>$<` contramap encoders, manual `Row` decoders, and the column list duplicated 4–6 times per table (insert tuple, update set list, select projection, conflict update). `BattingRow` alone has the column name list repeated four times across 28 fields. This is not sustainable as the schema grows, and it's the exact boilerplate rel8 exists to eliminate. If you're going to use raw hasql, fine — but be honest with yourself that you're choosing more boilerplate forever, and document that decision so future-you doesn't migrate again. My recommendation stands: switch to rel8 now while there are 14 tables, not 40.

**Effect interpreters throw IO exceptions instead of using `Effectful.Error.Static`.**
```haskell
runOrThrow io = E.liftIO io >>= \case
  Right a  -> pure a
  Left err -> E.liftIO (ioError (userError ("DB error: " <> show err)))
```
This throws away the type-level promise of effectful's effect rows. Either:
- Add `Error DBError :> es` to your effects and `throwError` instead of `ioError`, or
- Be explicit that DB failures are treated as IO exceptions and document the rationale.

Picking one and sticking with it matters more than which one.

**`Rational` in `Pelotero.Domain.Scoring` is precise, but JSON parses `Rational` via `Double`.** The aeson `FromJSON Rational` instance goes through `Scientific`/`Double`, so `0.1` in your config becomes `3602879701896397 % 36028797018963968`, not `1 % 10`. If you want exact rationals, parse from a `String` like `"0.5"` or a `[num, den]` array, not raw JSON numbers. Otherwise just use `Double` and stop pretending.

**No `katip`, no `crem`, no `hasql-migration`.** You rolled your own migration runner (it's correct, but `hasql-migration` already exists and is what Cheeblr-adjacent projects use). The draft state machine is plain functions instead of crem. Logging is `hPutStrLn stderr` and `TIO.putStrLn`. Whether these are acceptable in Phase 2 depends on what "Phase 2" means to you — but the divergence from the Cheeblr stack you wanted to mirror is widening, not closing.

**`fetchLogId :: Maybe Int64` and `playerRowId :: Maybe DbPlayerId` patterns** smell — you're using `Nothing` to mean "I haven't been inserted yet." That's the standard hack but it forces every caller to handle a case that's impossible after insert. Consider `data PlayerRow` (no id field) plus `data StoredPlayerRow = StoredPlayerRow !DbPlayerId !PlayerRow`. Splits the "before persisted" and "after persisted" types cleanly.

## Order of operations I'd suggest

1. Move the file (one line, makes the build progress to the next real error).
2. Delete `app/Combined.hs` if it's real.
3. Fix the two `error` calls so bad data doesn't crash the process.
4. **Decide rel8 vs raw hasql for real, and commit to it.** If raw hasql, write a small generator (TH or just a helper module) so each new table is ≤ 30 lines, not 200+. If rel8, port the existing 14 tables before adding more.
5. Then continue with Phase 2.

Want me to write a sample rel8 port of one of the larger tables (BoxscoreEntry, say) so you can compare the boilerplate side-by-side and decide?
```
