CREATE TABLE lineup_snapshot (
  id              BIGSERIAL    PRIMARY KEY,
  league_team_id  BIGINT       NOT NULL REFERENCES league_team(id),
  game_id         BIGINT       NOT NULL REFERENCES game(id),
  slot            TEXT         NOT NULL,
  player_id       BIGINT       NOT NULL REFERENCES player(id),
  snapshotted_at  TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
  UNIQUE (league_team_id, game_id, slot, player_id)
);

CREATE INDEX lineup_snapshot_game      ON lineup_snapshot(game_id);
CREATE INDEX lineup_snapshot_team_game ON lineup_snapshot(league_team_id, game_id);