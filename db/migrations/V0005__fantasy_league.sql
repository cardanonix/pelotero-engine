-- db/migrations/V0005__fantasy_league.sql
--
-- Fantasy league tables: config, teams, roster/lineup slots,
-- player rankings, and draft picks.

------------------------------------------------------------------------
-- League configuration
------------------------------------------------------------------------
CREATE TABLE league_config (
  id              BIGSERIAL   PRIMARY KEY,
  league_id       TEXT        NOT NULL UNIQUE,
  commissioner    TEXT        NOT NULL,
  status          TEXT        NOT NULL DEFAULT 'draft'
                              CHECK (status IN ('draft', 'active', 'closed')),
  scoring_config  JSONB       NOT NULL,
  roster_limits   JSONB       NOT NULL,
  lineup_limits   JSONB       NOT NULL,
  draft_auto      BOOLEAN     NOT NULL DEFAULT FALSE,
  draft_strategy  TEXT        NOT NULL DEFAULT 'serpentine'
                              CHECK (draft_strategy IN ('serpentine', 'experimental_snake')),
  draft_auto_at   TIMESTAMPTZ,
  scoring_start   TIMESTAMPTZ NOT NULL,
  scoring_end     TIMESTAMPTZ NOT NULL,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

------------------------------------------------------------------------
-- Fantasy teams within a league
------------------------------------------------------------------------
CREATE TABLE league_team (
  id              BIGSERIAL   PRIMARY KEY,
  league_config_id BIGINT     NOT NULL REFERENCES league_config(id),
  team_key        TEXT        NOT NULL,
  name            TEXT        NOT NULL,
  owner           TEXT        NOT NULL,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (league_config_id, team_key)
);

------------------------------------------------------------------------
-- Roster slots: full draft roster
--
-- One row per (team, player). The slot column is the RosterSlot text
-- code ('catcher', 'first', 's_pitcher', etc.). A player appears at
-- most once per team; the slot tells us where.
------------------------------------------------------------------------
CREATE TABLE roster_slot (
  id               BIGSERIAL   PRIMARY KEY,
  league_team_id   BIGINT      NOT NULL REFERENCES league_team(id),
  slot             TEXT        NOT NULL,
  player_id        BIGINT      NOT NULL REFERENCES player(id),
  created_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (league_team_id, player_id)
);

CREATE INDEX idx_roster_slot_team_slot
  ON roster_slot (league_team_id, slot);

------------------------------------------------------------------------
-- Lineup slots: active lineup for a scoring period
--
-- Same shape as roster_slot. A player in the lineup must also be on
-- the roster; we enforce that in application code rather than a FK
-- because the roster_slot PK is a surrogate, not (team, player).
------------------------------------------------------------------------
CREATE TABLE lineup_slot (
  id               BIGSERIAL   PRIMARY KEY,
  league_team_id   BIGINT      NOT NULL REFERENCES league_team(id),
  slot             TEXT        NOT NULL,
  player_id        BIGINT      NOT NULL REFERENCES player(id),
  created_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (league_team_id, player_id)
);

CREATE INDEX idx_lineup_slot_team_slot
  ON lineup_slot (league_team_id, slot);

------------------------------------------------------------------------
-- Player rankings: pre-draft preference list
------------------------------------------------------------------------
CREATE TABLE player_ranking (
  league_team_id   BIGINT      NOT NULL REFERENCES league_team(id),
  player_id        BIGINT      NOT NULL REFERENCES player(id),
  rank_slot        INT         NOT NULL CHECK (rank_slot > 0),
  updated_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (league_team_id, player_id),
  UNIQUE (league_team_id, rank_slot)
);

------------------------------------------------------------------------
-- Draft picks
------------------------------------------------------------------------
CREATE TABLE draft_pick (
  id               BIGSERIAL   PRIMARY KEY,
  league_config_id BIGINT      NOT NULL REFERENCES league_config(id),
  pick_number      INT         NOT NULL CHECK (pick_number > 0),
  league_team_id   BIGINT      NOT NULL REFERENCES league_team(id),
  player_id        BIGINT      NOT NULL REFERENCES player(id),
  picked_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (league_config_id, pick_number),
  UNIQUE (league_config_id, player_id)
);