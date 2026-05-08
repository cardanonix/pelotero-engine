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
Status check against the chase bundle: your "this week" list is fully landed. A.1 (ExternalId), A.6 (handChar), A.2 (Logging effect with the three interpreters), A.3 (typed DBError flowing through runDatabasePool), B.1 (canonical pitOuts, with WireFieldDiscrepancy warnings on disagreement), and B.3 (LineupSnapshot table, effect, orchestration, V0030 migration, Score reading from snapshots). The Score module's per-game scoring loop reading from getSnapshotForTeamGame is exactly the shape you sketched. Good.
What's left:

A.4: JSON instances out of Domain (newtype wrappers in Pelotero.DB.JsonB)
A.5: ProviderKeyed typeclass (collapses Player/Team/Game external-id boilerplate)
B.2: order_index columns on roster_slot and lineup_slot
C.1: idempotency-by-SHA in syncRosters and syncSchedule (complete)
C.2: single-SELECT scoring via date-range box queries
D.1/D.2: Draft state machine and AutoDraft port
E.1/E.2: CLI consolidation and old_src/ deletion

Honest take on ordering: none of these block each other. B.3 was the load-bearing piece and it's done. The rest is genuinely parallelizable cleanup, your "rest is mechanical" framing was right.
My recommendation is C.1 first, then A.5.
C.1 because it's roughly ten lines per function and produces a real operational change: today, re-running syncRosters against an unchanged payload still does the full team+player upsert and writes a fetch-log row. With the SHA short-circuit, re-runs become free for unchanged data, which makes cron-driven sync sane. The FetchLog already stores the SHA you need; you're using existing infrastructure.
A.5 second because it's the big satisfying collapse and the three callers (DB.Player, DB.Team, DB.Game plus their effect interpreters) have genuinely earned the abstraction. A.4 is honestly a lower-priority layering cleanup and orthogonal to A.5 since LeagueConfig has no external IDs and so isn't a ProviderKeyed candidate. Doing A.5 first lets the typeclass shape stabilize against three real callers before you add wrapper noise from A.4.
One pushback worth airing: B.2 (order_index) is in the plan but has zero current consumers. Adding the column now means a migration, four code touches, and a Hedgehog property for an invariant nobody reads. I'd defer B.2 until the first feature that actually wants positional batting order, and add the column as part of that feature's work. Landing it speculatively is the kind of thing that bit-rots and you discover six months later that the order field was never actually populated correctly because nothing tested it. Your call, but the plan's "today no consumer cares" framing is itself the argument for not doing it yet.
Want me to start on C.1? I'd write the complete updated Pelotero.Sync.Players and Pelotero.Sync.Schedule with the SHA short-circuit at the top of each, plus the Logging.logFM InfoS line for the skip case so the skip is observable. Should be a tight diff.


___________________
expect the same run/run_/run1 and NumberOfRowsAffected → NoReturning edits across the other DB modules (Game.hs, BoxscoreEntry.hs, LeagueConfig.hs, LeagueTeam.hs, RosterSlot.hs, LineupSlot.hs, LineupSnapshot.hs, PlayerRanking.hs, DraftPick.hs, FetchLog.hs). The pattern is identical: select → run, insert/update with Returning fld → run1, insert/update with NoReturning → run_. A bulk grep for NumberOfRowsAffected and for Tx.statement () \$ insert/Tx.statement () \$ update (without run) should turn up everywhere that needs touching.  