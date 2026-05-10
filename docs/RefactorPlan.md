## Refactor plan 2.0

I'm collapsing the work into five phases ordered by "blocks future work" rather than by feature area. Each item is small enough to land in one or two sittings.

### Phase A — Foundations

These all unblock other phases; do them first or you'll redo work.

**A.1 — `Pelotero.Provider.ExternalId` helper** (Issue 3)

One module exporting:
```
externalIdFromTeamId   :: TeamId   -> Text
externalIdFromPlayerId :: PlayerId -> Text
externalIdFromGameId   :: GameId   -> Text
externalIdToTeamId     :: Text -> Maybe TeamId
externalIdToPlayerId   :: Text -> Maybe PlayerId
externalIdToGameId     :: Text -> Maybe GameId
```

Replace every `T.pack . show . unTeamId` (and friends) with the helper. Sites: `Sync.Players.upsertAllTeams`, `Sync.Players.upsertAllPlayers`, `Sync.Schedule.upsertOneGame`, `Sync.Schedule.resolveTeam`, `Sync.Boxscores.syncOne`, `Sync.Boxscores.upsertEntries`. Add a Hedgehog roundtrip property: `externalIdToTeamId . externalIdFromTeamId === Just`.

**A.2 — `Pelotero.Effects.Logging`** (Issue 6)

Lift the Cheeblr Katip pattern: a `Logging` effect with `logFM`, `logItem`, `katipNamespace`, plus a broadcast scribe in production (stdout JSON + file rotating). Test interpreter is `runLoggingPure :: Eff (Logging : es) a -> Eff es (a, [LogLine])` collecting messages for assertion. Replace `Convert.logWarnings` (direct stderr IO) with `Logging.logFM`. The `logWarningsTo` Handle indirection becomes test-mode dead code; delete it.

**A.3 — Typed `DBError` via `Error` co-effect** (Issue 7)

Change `Database` effect's interpreter signature:
```
runDatabasePool
  :: (IOE :> es, Error DBError :> es)
  => Pool
  -> Eff (Database : es) a
  -> Eff es a
```
Delete `runOrThrow`. Callers that don't care about typed handling do `runError @DBError` at the top and either crash or log on `Left`. Callers that want to handle pool exhaustion specifically can pattern-match on `PoolUsageError`. Update every effect runner that consumes `Database` (Players, Teams, Games, BoxscoreEntry, FetchLog, LeagueConfig, LeagueTeam, RosterSlot, LineupSlot, PlayerRanking, DraftPick) to thread `Error DBError` through.

**A.4 — Layering: pull JSON instances out of Domain** (Issue 12)

Move `ToJSON`/`FromJSON` for `LeagueScoring`, `BattingMultipliers`, `PitchingMultipliers`, `RosterLimits`, `LineupLimits` out of `Pelotero.Domain.*`. Two reasonable shapes:

Option a (my preference): create `Pelotero.DB.JsonB` with newtype wrappers:
```
newtype JsonbScoring = JsonbScoring LeagueScoring
  deriving newtype (Show, Eq)
  -- ToJSON/FromJSON instances live HERE
```
Use the wrapper type in `LeagueConfig`'s `Column f` declarations; unwrap at the API boundary. Domain types stay pure.

Option b: an `Encoding` module that imports both Domain and Aeson. Less ideal because the orphan has to live somewhere and the wrapper version makes the layer explicit.

**A.5 — `Pelotero.DB.Common.ProviderKeyed`** (Issue 9)

Abstract the surrogate-id + external_id pattern over the row type. Sketch:

```haskell
class ProviderKeyed row where
  type RowId row
  type RowEntity row
  rowSchema           :: TableSchema (RowEntity row Name)
  externalIdSchema    :: TableSchema (ExternalIdE row Name)
  rowIdColumn         :: RowEntity row Expr -> Expr (RowId row)
  externalIdToRow     :: ExternalIdE row Expr -> Expr (RowId row)

upsertByExternalIdT
  :: ProviderKeyed row
  => ProviderName -> Text -> row -> Tx.Transaction (RowId row)

linkExternalIdT
  :: ProviderKeyed row
  => RowId row -> ProviderName -> Text -> Tx.Transaction ()

lookupByExternalIdT
  :: ProviderKeyed row
  => ProviderName -> Text -> Tx.Transaction (Maybe (RowId row))

getExternalIdT
  :: ProviderKeyed row
  => RowId row -> ProviderName -> Tx.Transaction (Maybe Text)
```

Then `Pelotero.DB.Player`, `.Team`, `.Game` each provide one `instance ProviderKeyed PlayerRow` (etc.) plus their entity-specific reads (`getActive`, `getByDate`, etc.). The boilerplate `linkExternalIdT/lookupByExternalIdT/getExternalIdT/upsertByExternalIdT` collapses to one definition. The test interpreters in `Effects.Players/Teams/Games` similarly collapse: they share a `ProviderKeyedStore row` shape and one `runProviderKeyedInMemory`.

This is the only abstraction that's earned its keep at three callers. Don't generalize further (LeagueConfig and LeagueTeam don't have external ids, DraftPick is append-only) — they stay bespoke.

**A.6 — Total `handChar`** (Issue 11)

Add to `Pelotero.Domain.Player`:
```
handChar :: Handedness -> Char
handChar = \case
  LeftHanded  -> 'L'
  RightHanded -> 'R'
  Switch      -> 'S'
```
Replace `T.head . renderHandedness` in `Sync.Players.upsertAllPlayers` with `handChar`. No partiality, totality lives in the type.

### Phase B — Correctness

These fix actual wrong behaviour, not just structural smells. B.3 is the load-bearing one for the entire engine being usable.

**B.1 — Canonical `pitOuts` in domain + discrepancy warnings** (Issue 10)

Per the rework above:
- `Pelotero.Domain.Stats.PitchingStats`: drop `pitInningsPitched`. Keep `pitOuts :: Maybe Int`.
- `Pelotero.MLB.Convert`: in `convertPitching`, parse both wire fields, compare, emit `WireFieldDiscrepancy` if they disagree; output `pitOuts = parseInningsPitched ip <|> wbpOuts`.
- `Pelotero.Domain.Scoring.scorePitching`: read `pitOuts` directly. Drop the `parseInningsPitched . pitInningsPitched` step.
- `Pelotero.Score.rowToPitchingStats`: drop the `renderInningsPitched <$>` reconstruction. Just pass outs through.
- DB schema: `_pInningsPitchedOuts :: Column f (Maybe Int32)` becomes the single source.

This removes the round-trip-through-Text and the redundancy footgun.

**B.2 — `order_index` column on `roster_slot` and `lineup_slot`** (Issue 13)

New migration `V0030__order_index.sql`:
```sql
ALTER TABLE roster_slot ADD COLUMN order_index INTEGER NOT NULL DEFAULT 0;
ALTER TABLE lineup_slot ADD COLUMN order_index INTEGER NOT NULL DEFAULT 0;
CREATE INDEX roster_slot_order ON roster_slot(league_team_id, slot, order_index);
CREATE INDEX lineup_slot_order ON lineup_slot(league_team_id, slot, order_index);
```

`RosterSlotRow` and `LineupSlotRow` get an `rsOrderIndex :: Int32` field. `addSlotT` sets it from a caller-supplied value or computes `MAX(order_index) + 1` within the slot. `getSlotsForTeamT` does `ORDER BY slot, order_index, player_id`. The Seq-based domain ordering now actually persists. Today no consumer cares about the order; tomorrow when batting orders matter, it's already there.

**B.3 — `lineup_snapshot` table + per-game-start snapshot writes + scoring reads from snapshots** (Issue 1, the big one)

Schema:
```sql
CREATE TABLE lineup_snapshot (
  id              BIGSERIAL PRIMARY KEY,
  league_team_id  BIGINT NOT NULL REFERENCES league_team(id),
  game_id         BIGINT NOT NULL REFERENCES game(id),
  slot            TEXT   NOT NULL,
  player_id       BIGINT NOT NULL REFERENCES player(id),
  order_index     INTEGER NOT NULL DEFAULT 0,
  snapshotted_at  TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (league_team_id, game_id, slot, player_id)
);
CREATE INDEX lineup_snapshot_game ON lineup_snapshot(game_id);
CREATE INDEX lineup_snapshot_team_game ON lineup_snapshot(league_team_id, game_id);
```

New module `Pelotero.DB.LineupSnapshot` mirrors the LineupSlot shape. New effect `Pelotero.Effects.LineupSnapshot`. Snapshot-writing logic in a new `Pelotero.Lineup.Snapshot` module:

```haskell
snapshotLineupsForGame
  :: ( LineupSlot     :> es
     , LineupSnapshot :> es
     , LeagueTeam     :> es
     , Clock          :> es
     , Logging        :> es
     )
  => DbGameId -> Eff es ()
```

Called from a CLI subcommand (`pelotero snapshot lineups --on-date 2025-04-15`) that runs on a cron schedule before the day's earliest game starts. For every active `league_team`, copy current `lineup_slot` rows into `lineup_snapshot` keyed by `(league_team_id, game_id)`. Idempotent: re-running before game start updates; re-running after silently skips per the unique constraint.

`Pelotero.Score.scoreOneTeam` switches its source: instead of `LS.getSlotsForTeam`, use a new `LineupSnapshot.getSnapshotForTeamGame :: DbLeagueTeamId -> DbGameId -> Eff es [LineupSlotRow]`. The scoring loop is now per-game (which it should be anyway): for each game in the period, get the snapshot for this team for that game, score that game's stats against that snapshot, sum.

This makes the score function correct even if owners shuffle lineups mid-period. Re-running scoring at any time gives the same answer. This is the precondition for everyone trusting the engine.

Note: this depends on B.2 (order_index) only if you want batting-order positional scoring; for current scoring it's independent. Land B.2 first if you want to do them as one migration.

### Phase C — Performance and operational hygiene

**C.1 — Idempotency by checksum in syncRosters and syncSchedule** (Issue 4)

Both `syncRosters` and `syncSchedule` get a guard at the top:

```haskell
syncRosters provider scope payloadSha teams players = do
  prior <- getLastFetch provider "active-rosters" scope
  case prior of
    Just FetchLogRow { fetchLogPayloadSha256 = oldSha }
      | oldSha == payloadSha -> do
          logFM InfoS $ "rosters: payload unchanged, skipping; sha=" <> payloadSha
          pure SyncResult { syncTeamsUpserted = 0, syncPlayersUpserted = 0
                          , syncFetchSha256 = payloadSha }
    _ -> do
      ... existing path ...
```

Same shape for `syncSchedule` with `"schedule"` resource. Five lines each.

**C.2 — Single-SELECT scoring** (Issue 8)

Replace `Pelotero.Score.buildPlayerMaps` (currently `2 × n_games` transactions) with a single repository function in `Pelotero.DB.BoxscoreEntry`:

```haskell
getBattingForDateRangeT
  :: Day -> Day -> Tx.Transaction (Map.Map DbPlayerId [BattingRow])
getBattingForDateRangeT startDay endDay = do
  rows <- select $ do
    g <- each gameSchema
    b <- each battingSchema
    where_ $ _gameId g ==. _bGameId b
        &&. _gameGameDate g >=. lit startDay
        &&. _gameGameDate g <=. lit endDay
    pure b
  pure $ Map.fromListWith (++) [(battingPlayerId r, [r]) | r <- map fromBattingResult rows]
```

Mirror for pitching. `Pelotero.Effects.BoxscoreEntry` adds two operations; in-memory interpreter does a List filter+groupBy. `Score.buildPlayerMaps` becomes two effect calls instead of `2 * n` transactions. For a season-long period this is the difference between 4800 transactions and 2.

### Phase D — Draft

**D.1 — `Pelotero.Draft` with crem state machine** (Issue 5)

New module, NOT in `Pelotero.Domain` (it has effect dependencies):

```haskell
-- States
data DraftState
  = WaitingToStart
  | Drafting !DraftContext
  | Complete !DraftSummary

data DraftContext = DraftContext
  { dcLeague        :: !DbLeagueConfigId
  , dcOrder         :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcRemaining     :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcAvailable     :: !(Set DbPlayerId)
  , dcPicksMade     :: !Int
  }

-- Commands and events; let crem derive the singletons via Generic
data DraftCommand
  = StartDraft DbLeagueConfigId
  | MakePick   DbLeagueTeamId DbPlayerId
  | AutoPick   DbLeagueTeamId
  | EndDraft

data DraftEvent
  = DraftStarted DbLeagueConfigId [DbLeagueTeamId]
  | PickRecorded DbLeagueTeamId DbPlayerId DraftPickNumber
  | DraftCompleted DraftSummary
```

Use crem's `StateMachine` type to declare valid transitions. Effects: `LeagueConfig`, `LeagueTeam`, `PlayerRanking`, `DraftPick`, `Players`, `Logging`, `Clock`. `MakePick` consults the team's `PlayerRanking` to validate the pick is sane (not already drafted, on the available list, picker's turn). `AutoPick` chooses by `PlayerRanking` head, falling back to `extendRankingsWithUnranked`.

The legacy `(state, Maybe String)` "did anything change" trick dies. crem's transition function returns `Either DraftError NewState`.

**D.2 — Port AutoDraft as `pelotero draft run`**

Once the state machine exists, a CLI app instantiates `WaitingToStart`, sends `StartDraft`, then loops `AutoPick` until `Complete`. Compare summary against legacy `AutoDraft.hs` output as an acceptance test.

### Phase E — Apps and cleanup

**E.1 — `app/*.hs` mains**

```
app/sync-rosters.hs       -> pelotero sync rosters --season 2025
app/sync-schedule.hs      -> pelotero sync schedule --from 2025-04-01 --to 2025-04-07
app/sync-boxscores.hs     -> pelotero sync boxscores --from 2025-04-01 --to 2025-04-07
app/snapshot-lineups.hs   -> pelotero snapshot lineups --on-date 2025-04-15
app/score.hs              -> pelotero score --league-id <id>
app/draft-run.hs          -> pelotero draft run --league-id <id>
app/league-validate.hs    -> pelotero league validate --league-id <id>
```

Library carries everything; app mains are thin parsers + effect runner stacks.

**E.2 — Delete `old_src/`**

Only after every executable above is passing acceptance tests against fixture data and at least one real-world boxscore comparison.


## What I'd actually do this week

1. Phase A.1 (external-id helper) and A.6 (handChar). Both small, both unblock everything else, both removable as open issues by EOD.
2. Phase A.2 (Logging) and A.3 (typed DBError). They want to go together because the effect-runner stack changes.
3. Phase B.1 (canonical pitOuts). Small but touches Score, Convert, Domain.Stats, DB.BoxscoreEntry. Do this before any other Score work.
4. Phase B.3 (lineup snapshots). The big one. Land it and re-score a few weeks of historical data; verify the numbers don't drift on re-runs after lineup edits.

After that the rest is mechanical. The plan above gets you to a state where your engine is correct, observable, idempotent, and ready to delete the old code.

___________________

Updated progress:

The Draft consolidation is done. Single transition implementation (the crem state machine), no duplication, the Show SomeDraftStateG instance you added makes future debugging easier. The two new Hedgehog properties — rejection invariance and "Complete is absorbing" — are stronger than the original agreement test, because they assert the machine respects rules without reference to a second source. If you ever extend the state machine with new transitions, those properties keep earning their keep automatically.
The partial-function fix in Score is also done. lltId :: DbLeagueTeamId and llcId :: DbLeagueConfigId are total. The error "scoreOneTeam: team without surrogate id" is gone. The Loaded* types correctly model the read-path-totality invariant.
Where you are on the original plan:

A.1 ExternalId — done
A.2 Logging — done
A.3 typed DBError — done
A.4 JsonB wrappers — done
A.5 ProviderKeyed — done
A.6 handChar — done
B.1 canonical pitOuts — done
B.3 lineup snapshots — done
C.1 SHA idempotency — done (rosters, schedule, boxscores)
C.2 single-SELECT scoring — done
D.1/D.2 Draft state machine and AutoDraft — done and now consolidated

Plus two correctness items not on the original list:

Score.scoreOneTeam partial function — done
Loaded* types for read-path totality — done (LeagueTeam, LeagueConfig)

Remaining:

B.2 order_index — I'd still defer until a feature actually needs it
E.1 CLI consolidation — app/Main.hs is still a stub; pelotero <subcommand> shape not yet wired
E.2 delete old_src/ — once E.1 lands and you're confident nothing in the legacy tree is still load-bearing

I'd take a beat before E.1. You've made a lot of changes in a short window, all of them green now. Sit on it for a day, run the binary against a real season's worth of MLB fixtures end-to-end (sync → snapshot → score), make sure nothing funny shows up that the unit tests don't catch. Then E.1 is mostly mechanical optparse-applicative wiring, and you can do it without dragging behind half-baked correctness work.
Want to run an end-to-end smoke against real fixtures next, or move straight to E.1?

another update:
Reviewed the plan and the code. Picking up where the prior thread left off, with the question "smoke test vs E.1" sitting on the table.
On the smoke test vs E.1 question: smoke test first, and the plan is correct to push for it. E.1 is mechanical optparse-applicative wiring. If the engine is wrong end-to-end, you don't want to discover that while also debugging argument parsing. The smoke test exercises the actual integration: fetch → upsert → snapshot → score → re-score-after-edit. If that's green, E.1 becomes a one-evening job.
But before you run that smoke, a few honest observations on the code as it stands:
Bugs and smells worth fixing now

pelotero-engine.cabal lists effectful twice in the library build-depends. Cabal will accept it, but it's noise. Drop the bare , effectful line.
Pelotero.DB.Pool.runTransaction always uses TxS.Write mode. Every getById, getAll, getByDateRange is opened as a write transaction. Functionally fine but it wastes the read/write distinction Postgres gives you for free. Either thread the mode through or split into runTransactionR / runTransactionW. Not load-bearing today; will matter the moment you have concurrent readers.
N+1 in scoring snapshot reads. scoreOneTeam does traverse (scoreTeamForGame ...) gameIds, and scoreTeamForGame calls LSnap.getSnapshotForTeamGame ltid gid. So for L leagues × T teams × G games you do LTG snapshot queries. C.2 fixed this for batting/pitching with getBattingForDateRange; you didn't do the equivalent for snapshots. A getSnapshotsForDateRange :: Day -> Day -> Tx.Transaction [LineupSnapshotRow] joined to game on date, then grouped client-side by (league_team_id, game_id), would collapse all of them to one. For a season it's the difference between thousands of round-trips and one.
Pelotero.Sync.Boxscores.upsertEntries silently drops boxscore entries for players not yet in the DB (the Nothing branch on Players.lookupPlayerByExternalId). At minimum log a warning. Right now an unsynced player will just vanish from scoring with no signal.
runDatabasePool calls logFM ErrorS before throwError. If your Logging interpreter ever does anything that can fail (it can't with the current Katip one, but the Capture one writes through an IORef which can't fail either, so practically OK), the original DBError gets masked. Low-priority but the pattern of "log then throw" inside an effect handler is worth being aware of.
The plan deferring B.2 is fine for current scoring, but be honest with yourself: any positional batting-order scoring (e.g., leadoff bonus, cleanup penalty) needs order_index in lineup_snapshot too, not just lineup_slot. If you ever turn that on, you have to backfill snapshots, and you can't because the original lineup ordering is lost the moment the owner edits. Document this explicitly, or the deferral becomes a debt note nobody re-reads.

Smoke test scenarios I'd actually run
The point of a smoke isn't "does it not crash." It's "does the engine produce numbers I can defend." Scenarios in priority order:

Sync a known week of games end-to-end (rosters → schedule → boxscores), then score against fixtures where you can hand-verify a single player's points against the multipliers. One starting pitcher's QS bonus is enough to expose 10 bugs.
Re-run scoring with no data changes. Numbers must be byte-identical. If they drift, you have non-determinism somewhere (UTC handling, ordering, Map iteration on stale snapshots).
Edit a lineup mid-period, re-snapshot, re-score. Games before the edit must score against the old snapshot, games after against the new. This is the whole point of B.3 and the only real test of it.
Run boxscore sync twice with no upstream change. SHA idempotency must short-circuit. Confirm via the boxGamesUnchanged count, not just absence of errors.
Drop one game's snapshot, re-score. Team's total should decrease by exactly that game's contribution. If it doesn't, your snapshot read is broken or your merge is double-counting.

Once those are green, E.1 is safe. Until then it's premature.
One last thing: you said you want full files when you ask for code. Nothing here required code edits. When you're ready to address #1, #3, or #4 above, ask and I'll cut the files.