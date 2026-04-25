-- Initial schema for pelotero-engine.
--
-- Design notes:
--   * Surrogate primary keys (BIGSERIAL) on player, team, game. The natural
--     identifiers from upstream providers (MLB, future stat aggregators) live
--     in side tables (player_external_id, team_external_id, game_external_id).
--     This decouples the schema from any one provider and accommodates a
--     future migration off MLB.
--   * Free-form TEXT for `provider` — the application layer controls which
--     provider names exist; the DB just stores them. Adding a provider is
--     not a migration.
--   * Stat fields are nullable. NULL means "this player did not bat/pitch in
--     this game." Scoring queries COALESCE to zero where appropriate.
--   * Two stat tables (batting, pitching) rather than one combined table.
--     Two-way players (Ohtani) get a row in each. Scoring queries don't have
--     to filter "where stats_kind = 'batting'."

--------------------------------------------------------------------------------
-- Teams
--------------------------------------------------------------------------------

CREATE TABLE team (
  id                    BIGSERIAL    PRIMARY KEY,
  name                  TEXT         NOT NULL,
  abbreviation          TEXT         NOT NULL,
  location_name         TEXT         NOT NULL,
  last_synced_provider  TEXT,
  last_synced_at        TIMESTAMPTZ,
  created_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  updated_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW()
);

CREATE TABLE team_external_id (
  team_id      BIGINT       NOT NULL REFERENCES team(id) ON DELETE CASCADE,
  provider     TEXT         NOT NULL,
  external_id  TEXT         NOT NULL,
  fetched_at   TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  PRIMARY KEY (provider, external_id),
  UNIQUE (team_id, provider)
);

CREATE INDEX team_external_id_team_id_idx
  ON team_external_id (team_id);

--------------------------------------------------------------------------------
-- Players
--------------------------------------------------------------------------------

CREATE TABLE player (
  id                    BIGSERIAL    PRIMARY KEY,
  first_name            TEXT         NOT NULL,
  last_name             TEXT         NOT NULL,
  name_slug             TEXT         NOT NULL,
  position              TEXT,
  bat_side              CHAR(1),
  pitch_hand            CHAR(1),
  active                BOOLEAN      NOT NULL,
  current_team_id       BIGINT       REFERENCES team(id),
  last_synced_provider  TEXT,
  last_synced_at        TIMESTAMPTZ,
  created_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  updated_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  CONSTRAINT player_bat_side_valid   CHECK (bat_side   IS NULL OR bat_side   IN ('L','R','S')),
  CONSTRAINT player_pitch_hand_valid CHECK (pitch_hand IS NULL OR pitch_hand IN ('L','R','S'))
);

CREATE TABLE player_external_id (
  player_id    BIGINT       NOT NULL REFERENCES player(id) ON DELETE CASCADE,
  provider     TEXT         NOT NULL,
  external_id  TEXT         NOT NULL,
  fetched_at   TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  PRIMARY KEY (provider, external_id),
  UNIQUE (player_id, provider)
);

CREATE INDEX player_external_id_player_id_idx
  ON player_external_id (player_id);

CREATE INDEX player_active_idx
  ON player (last_name, first_name)
  WHERE active = TRUE;

CREATE INDEX player_team_idx
  ON player (current_team_id)
  WHERE current_team_id IS NOT NULL;

--------------------------------------------------------------------------------
-- Games
--------------------------------------------------------------------------------

CREATE TABLE game (
  id                    BIGSERIAL    PRIMARY KEY,
  game_date             DATE         NOT NULL,
  away_team_id          BIGINT       NOT NULL REFERENCES team(id),
  home_team_id          BIGINT       NOT NULL REFERENCES team(id),
  last_synced_provider  TEXT,
  last_synced_at        TIMESTAMPTZ,
  created_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  updated_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW()
);

CREATE TABLE game_external_id (
  game_id      BIGINT       NOT NULL REFERENCES game(id) ON DELETE CASCADE,
  provider     TEXT         NOT NULL,
  external_id  TEXT         NOT NULL,
  fetched_at   TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  PRIMARY KEY (provider, external_id),
  UNIQUE (game_id, provider)
);

CREATE INDEX game_external_id_game_id_idx
  ON game_external_id (game_id);

CREATE INDEX game_date_idx
  ON game (game_date);

--------------------------------------------------------------------------------
-- Stats
--------------------------------------------------------------------------------

CREATE TABLE game_player_batting (
  game_id                    BIGINT  NOT NULL REFERENCES game(id)   ON DELETE CASCADE,
  player_id                  BIGINT  NOT NULL REFERENCES player(id) ON DELETE CASCADE,
  team_id                    BIGINT           REFERENCES team(id),
  games_played               INT,
  plate_appearances          INT,
  at_bats                    INT,
  runs                       INT,
  hits                       INT,
  doubles                    INT,
  triples                    INT,
  home_runs                  INT,
  rbi                        INT,
  base_on_balls              INT,
  intentional_walks          INT,
  strike_outs                INT,
  stolen_bases               INT,
  caught_stealing            INT,
  hit_by_pitch               INT,
  sac_bunts                  INT,
  sac_flies                  INT,
  ground_into_double_play    INT,
  ground_into_triple_play    INT,
  left_on_base               INT,
  total_bases                INT,
  fly_outs                   INT,
  ground_outs                INT,
  catchers_interference      INT,
  pickoffs                   INT,
  created_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (game_id, player_id)
);

CREATE INDEX game_player_batting_player_idx
  ON game_player_batting (player_id);

CREATE TABLE game_player_pitching (
  game_id                       BIGINT  NOT NULL REFERENCES game(id)   ON DELETE CASCADE,
  player_id                     BIGINT  NOT NULL REFERENCES player(id) ON DELETE CASCADE,
  team_id                       BIGINT           REFERENCES team(id),
  games_played                  INT,
  games_started                 INT,
  games_finished                INT,
  complete_games                INT,
  shutouts                      INT,
  wins                          INT,
  losses                        INT,
  saves                         INT,
  save_opportunities            INT,
  holds                         INT,
  blown_saves                   INT,
  innings_pitched_outs          INT,
  batters_faced                 INT,
  number_of_pitches             INT,
  strikes                       INT,
  balls                         INT,
  hits                          INT,
  doubles                       INT,
  triples                       INT,
  home_runs                     INT,
  runs                          INT,
  earned_runs                   INT,
  strike_outs                   INT,
  base_on_balls                 INT,
  intentional_walks             INT,
  hit_batsmen                   INT,
  wild_pitches                  INT,
  balks                         INT,
  pickoffs                      INT,
  fly_outs                      INT,
  ground_outs                   INT,
  air_outs                      INT,
  inherited_runners             INT,
  inherited_runners_scored      INT,
  stolen_bases                  INT,
  caught_stealing               INT,
  at_bats                       INT,
  rbi                           INT,
  sac_bunts                     INT,
  sac_flies                     INT,
  catchers_interference         INT,
  passed_ball                   INT,
  created_at                    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at                    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (game_id, player_id)
);

CREATE INDEX game_player_pitching_player_idx
  ON game_player_pitching (player_id);

--------------------------------------------------------------------------------
-- Provider fetch log
--------------------------------------------------------------------------------

CREATE TABLE provider_fetch_log (
  id              BIGSERIAL    PRIMARY KEY,
  provider        TEXT         NOT NULL,
  resource        TEXT         NOT NULL,
  scope           TEXT         NOT NULL,
  fetched_at      TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  payload_sha256  TEXT         NOT NULL,
  record_count    INT          NOT NULL
);

CREATE INDEX provider_fetch_log_lookup_idx
  ON provider_fetch_log (provider, resource, scope, fetched_at DESC);