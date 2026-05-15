# ⚾ Pelotero Engine ⚾

[![Haskell CI using Nix Flake](https://github.com/cardanonix/pelotero-engine/actions/workflows/haskell.yml/badge.svg)](https://github.com/cardanonix/pelotero-engine/actions/workflows/haskell.yml)

Pelotero is a fantasy baseball engine. It ingests data from the MLB Stats API, persists it in PostgreSQL, snapshots league lineups per game, scores teams against a league's scoring rules, and runs auto-drafts. The codebase is a single Haskell binary (`pelotero`) with an `effectful`-based effect layer over `rel8`/`hasql`, Katip structured logging, and a `crem`-typed draft state machine. Licensed under the GNU AGPLv3 or later.

## Architecture

The code is layered:

- **`Pelotero.Domain.*`** : pure data types (players, teams, games, rosters, lineups, scoring, draft). No effect dependencies, no Aeson. Hedgehog-testable in isolation.
- **`Pelotero.MLB.*`** : wire types and conversion. `Wire.*` parses upstream JSON; `Convert` produces domain values plus a list of `ConvertWarning`s rather than halting on the first malformed record. `Fetch` is the production HTTP path.
- **`Pelotero.DB.*`** : `rel8` schemas, transaction-level CRUD, and a `ProviderKeyed` typeclass that shares the external-id linking pattern across `Player`, `Team`, and `Game`. The migration runner is custom (`hasql-migration` is broken against `crypton`) and treats migration files as immutable post-apply: each file is SHA-256 hashed on first apply, subsequent runs verify the hash.
- **`Pelotero.Effects.*`** : dynamic `effectful` effects with both DB-backed and in-memory interpreters. The in-memory interpreters mimic `BIGSERIAL` via an `IORef`-held counter so property tests don't need Postgres.
- **`Pelotero.Sync.*`** : idempotent ingestion pipelines for rosters, schedule, and boxscores. All three short-circuit on SHA-256 payload match against `provider_fetch_log`.
- **`Pelotero.Lineup.Snapshot`** : freezes each team's lineup into `lineup_snapshot` keyed by `(league_team_id, game_id)`. First-snapshot-wins via `ON CONFLICT DO NOTHING` plus a pre-check. Fixes the correctness bug where late lineup edits used to retroactively change historical scores.
- **`Pelotero.Score`** : reads lineup snapshots (not the live lineup) and boxscore rows via two date-range queries per scoring call, independent of period length. `Points` is `Rational`, not `Double`, so a season total agrees with the sum of game lines.
- **`Pelotero.Draft`** + **`Pelotero.Draft.Machine`** : the pure transition logic for the draft. `crem` enforces the state topology at compile time; illegal transitions are type errors. `Pelotero.Draft.Run` is the effectful auto-draft loop.

The CLI in `app/Main.hs` is one binary with subcommands; everything else is library code.

## Quick start

From a clean clone to a working dev environment:

```
git clone https://github.com/cardanonix/pelotero-engine
cd pelotero-engine
nix develop
sops-bootstrap                     # generate age key + encrypted secrets
pg-start                           # local PostgreSQL on port 5433
cabal run pelotero -- db check     # apply migrations
```

After that, every subsequent shell entry is just `nix develop`. The shell hook decrypts `PGPASSWORD` from sops if a key is available; otherwise the password defaults to the bootstrap fallback (which you should rotate via `pg-rotate-credentials`).

## Dev shell tools

`nix develop` provides scripted access to everything you need.

**Database (PostgreSQL on port 5433):**

- `pg-start` / `pg-stop` / `pg-cleanup` : lifecycle
- `pg-connect` : psql into the project database
- `pg-backup` / `pg-restore <file>` : timestamped backups under `~/.local/share/pelotero-engine/backups`
- `pg-rotate-credentials` : generate a new password; update via `sops secrets/pelotero-engine.yaml`
- `pg-stats` : size, connection count, per-schema breakdown
- `with-db <cmd>` : run a command with DB env vars in scope

**Secrets (sops + age, derived from your SSH key):**

- `sops-init-key` : derive an age key from `~/.ssh/id_ed25519`
- `sops-bootstrap` : create `.sops.yaml` and `secrets/pelotero-engine.yaml` with a random DB password
- `sops-status` : show key and decryption state
- `sops-get <key>` / `sops-exec <cmd>` : single-value lookup or env-in-scope execution

**Development:**

- `pe-dev` : ensure DB is up, print connection info
- `pe-deploy` : tmux session with DB stats and dev panes
- `pe-stop` : backup, stop DB, kill tmux session
- `./tui` : project TUI (build, run, test, DB shortcuts); see below

**Code context (for LLM-assisted workflows):**

- `generate-manifest` : scan source into `script/manifest.json`
- `compile-manifest` : bundle source files for review
- `llm-context` : diff-aware context bundle from a git ref
- `manifest-tui` : interactive front-end for the above

## Building and running

The repo ships a small TUI wrapper:

```
./tui                  # interactive (use inside `nix develop`; requires gum)
./tui --build-all      # CI-friendly: nix-build every cabal executable
./tui --help
```

Plain cabal works too:

```
cabal build all
cabal run pelotero -- <subcommand>
```

## CLI

Every command runs migrations first. Logs are JSON on stdout via Katip.

```
cabal run pelotero -- db check
cabal run pelotero -- sync rosters --season 2025
cabal run pelotero -- sync schedule --from 2025-04-01 --to 2025-04-07
cabal run pelotero -- sync boxscores --from 2025-04-01 --to 2025-04-07
cabal run pelotero -- snapshot lineups --on-date 2025-04-01
cabal run pelotero -- score --league-id 1
cabal run pelotero -- draft run --league-id 1
```

All sync commands are idempotent. They hash the upstream payload and short-circuit if it matches the most recently logged SHA for the same scope. Boxscore syncs always pay the HTTP cost (the bytes have to be fetched to be hashed) but skip parse, convert, upsert, and log on a match.

Operational ordering for a fresh ingest: `sync rosters` first, then `sync schedule` for a date range, then `sync boxscores` for the same range. `snapshot lineups` and `score` operate on whatever league data is already in the DB.

## Database configuration

Connection parameters via standard libpq env vars plus two tunables. The dev shell sets sensible defaults:

```
PGHOST                        # default: $PGDATA (local socket)
PGPORT                        # default: 5433
PGDATABASE                    # default: pelotero-engine
PGUSER                        # default: $(whoami)
PGPASSWORD                    # default: decrypted from secrets/pelotero-engine.yaml
PELOTERO_DB_POOL_SIZE         # default: 10
PELOTERO_DB_ACQUIRE_TIMEOUT   # default: 10.0 (seconds)
```

Migrations live in `db/migrations/V<NNNN>__<desc>.sql` and are applied on every command. Don't edit applied migrations: create a new file, or the hash check refuses to run.

## Testing

Two test suites:

```
cabal test pelotero-test                  # unit + property tests, no DB
cabal test pelotero-integration-test      # DB-backed; requires pg-start
```

The pure layers are Hedgehog-testable directly. The effectful layers ship in-memory interpreters (`runPlayersInMemory`, `runTeamsInMemory`, `runGamesInMemory`, `runFetchLogInMemory`, etc.) that mimic DB semantics including surrogate-id assignment without spinning up Postgres. The integration suite covers the rel8 path end-to-end against a real database.

## Contributing

Issues and PRs welcome. The code is opinionated: pure-first, effects-at-the-edge, Aeson-free in `Domain`, no orphan instances outside `Pelotero.DB.Rel8Instances`, idempotency wherever a sync touches upstream data. Module-level design decisions are documented in `pelotero.chase` annotations.

Made with ⚾ by Harry Pray IV.

Copyright (C) 2024-2026 Harry Pray IV. Distributed under the GNU AGPLv3 or later.