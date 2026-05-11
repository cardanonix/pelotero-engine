#!/usr/bin/env bash
#
#
# Fetch real responses from the MLB statsapi, abridge each one to a pinned set
# of teams / players / games, and write commit-ready fixtures to
# integration-test/fixtures/mlb/.
#
#
# Requires: curl, jq

set -euo pipefail

# ---------------------------------------------------------------------------
# Config (override via env vars)
# ---------------------------------------------------------------------------

OUT_DIR="${OUT_DIR:-integration-test/fixtures/mlb}"
SEASON="${SEASON:-2025}"
START_DATE="${START_DATE:-2025-04-01}"
END_DATE="${END_DATE:-2025-04-07}"

# Players to pin. The script filters players-<SEASON>.json down to these IDs,
# and filters each boxscore's `players` map to just their entries. Add IDs as
# new tests want them.
#
#   514888  Jose Altuve        (Astros, 2B)
#   670541  Yordan Alvarez     (Astros, OF)
#   624413  Pete Alonso        (Mets,   1B)
#   596019  Francisco Lindor   (Mets,   SS)
PIN_PLAYERS="${PIN_PLAYERS:-514888 670541 624413 596019}"

# ---------------------------------------------------------------------------
# Tools
# ---------------------------------------------------------------------------

BASE="https://statsapi.mlb.com"
CURL=(curl --fail -sS --max-time 30)

need() {
  command -v "$1" >/dev/null 2>&1 || {
    printf 'missing required tool: %s\n' "$1" >&2
    exit 1
  }
}
need curl
need jq

log()  { printf '%s\n' "$*" >&2; }
step() { printf '\n----- %s -----\n' "$*" >&2; }

mkdir -p "$OUT_DIR"

ID_ARRAY="[$(echo "$PIN_PLAYERS" | tr -s ' \t\n' ',' | sed 's/,$//')]"

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------

step "CLEANUP"
log "removing existing fixture files in $OUT_DIR"
rm -f "$OUT_DIR"/teams-*.json \
      "$OUT_DIR"/players-*.json \
      "$OUT_DIR"/schedule-*.json \
      "$OUT_DIR"/boxscore-*.json

# ---------------------------------------------------------------------------
# Teams: keep all 30
# ---------------------------------------------------------------------------

step "TEAMS"
TEAMS_URL="${BASE}/api/v1/teams?sportId=1&season=${SEASON}"
log "GET $TEAMS_URL"
"${CURL[@]}" "$TEAMS_URL" \
  | jq 'del(.copyright)' \
  > "$OUT_DIR/teams-${SEASON}.json"
log "wrote teams-${SEASON}.json ($(jq '.teams | length' "$OUT_DIR/teams-${SEASON}.json") teams)"

# ---------------------------------------------------------------------------
# Players: filter to PIN_PLAYERS
# ---------------------------------------------------------------------------

step "PLAYERS"
PLAYERS_URL="${BASE}/api/v1/sports/1/players?activeStatus=ACTIVE&season=${SEASON}"
log "GET $PLAYERS_URL"
"${CURL[@]}" "$PLAYERS_URL" \
  | jq --argjson ids "$ID_ARRAY" '
        del(.copyright)
      | .people |= map(select(.id as $i | $ids | index($i)))
    ' \
  > "$OUT_DIR/players-${SEASON}.json"

GOT_PLAYERS=$(jq '.people | length' "$OUT_DIR/players-${SEASON}.json")
WANT_PLAYERS=$(echo "$PIN_PLAYERS" | wc -w | tr -d ' ')
log "wrote players-${SEASON}.json (${GOT_PLAYERS} of ${WANT_PLAYERS} pinned players present in the active-roster response)"
if [ "$GOT_PLAYERS" -lt "$WANT_PLAYERS" ]; then
  log "  WARNING: some pinned IDs were not returned."
  log "    pinned: $PIN_PLAYERS"
  log "    got:    $(jq -r '[.people[].id|tostring] | join(" ")' "$OUT_DIR/players-${SEASON}.json")"
fi

# ---------------------------------------------------------------------------
# Schedule: keep games involving any pinned player's currentTeam
# ---------------------------------------------------------------------------

step "SCHEDULE"
TEAM_IDS=$(jq -r '.people[].currentTeam.id' "$OUT_DIR/players-${SEASON}.json" \
            | sort -un | paste -sd, -)
log "filtering schedule to games involving team ids: [${TEAM_IDS}]"

SCHED_URL="${BASE}/api/v1/schedule/games/?language=en&sportId=1&startDate=${START_DATE}&endDate=${END_DATE}"
SCHED_FILE="$OUT_DIR/schedule-${START_DATE}-${END_DATE}.json"
log "GET $SCHED_URL"
"${CURL[@]}" "$SCHED_URL" \
  | jq --argjson tids "[${TEAM_IDS}]" '
        del(.copyright)
      | .dates |= (
          map(.games |= map(select(
            (.teams.away.team.id as $a | $tids | index($a)) or
            (.teams.home.team.id as $h | $tids | index($h))
          )))
          | map(select(.games | length > 0))
        )
    ' \
  > "$SCHED_FILE"

GAME_PKS=$(jq -r '[.dates[].games[].gamePk] | join(" ")' "$SCHED_FILE")
GAME_COUNT=$(echo "$GAME_PKS" | wc -w | tr -d ' ')
log "wrote $(basename "$SCHED_FILE") (${GAME_COUNT} games)"

# ---------------------------------------------------------------------------
# Boxscores: one per game in the filtered schedule
# ---------------------------------------------------------------------------

step "BOXSCORES"
if [ "$GAME_COUNT" -eq 0 ]; then
  log "no games to fetch boxscores for."
else
  for gpk in $GAME_PKS; do
    BOX_URL="${BASE}/api/v1/game/${gpk}/boxscore"
    log "GET $BOX_URL"
    "${CURL[@]}" "$BOX_URL" \
      | jq --argjson ids "$ID_ARRAY" '
            del(.copyright)
          | .teams.away.players |= (
              to_entries
              | map(select(.value.person.id as $i | $ids | index($i)))
              | from_entries
            )
          | .teams.home.players |= (
              to_entries
              | map(select(.value.person.id as $i | $ids | index($i)))
              | from_entries
            )
        ' \
      > "$OUT_DIR/boxscore-${gpk}.json"
    awayN=$(jq '.teams.away.players | length' "$OUT_DIR/boxscore-${gpk}.json")
    homeN=$(jq '.teams.home.players | length' "$OUT_DIR/boxscore-${gpk}.json")
    log "  boxscore-${gpk}.json: ${awayN} away + ${homeN} home pinned players"
  done
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

step "SUMMARY"
log ""
log "Fixtures written to: $OUT_DIR"
log ""
log "Pinned players:"
jq -r '.people[] | "  \(.id)  \(.fullName)  (\(.currentTeam.name))"' \
  "$OUT_DIR/players-${SEASON}.json" >&2
log ""
log "Pinned teams (currentTeam of each pinned player):"
jq -r '.people[].currentTeam | "  \(.id)  \(.name)"' \
  "$OUT_DIR/players-${SEASON}.json" | sort -un >&2
log ""
log "Discovered games (${GAME_COUNT}):"
for gpk in $GAME_PKS; do
  jq -r --arg id "$gpk" '
    .dates[].games[] | select((.gamePk|tostring) == $id)
    | "  \(.gamePk)  \(.officialDate)  \(.teams.away.team.name) @ \(.teams.home.team.name)"
  ' "$SCHED_FILE" >&2
done
log ""
log "NOTE: integration test specs currently reference fictional game IDs"
log "      (778001, 778002) and assume specific upsert counts. After this"
log "      run, the SyncSchedule and SyncBoxscores specs will fail. The"
log "      cleanest refactor pattern: fetch the schedule fixture inside the"
log "      test, drive boxscore sync from that game list, and assert"
log "      structural invariants (>0 games, no errors) instead of fixed"
log "      counts. Ping back when you want the spec refactor."
log ""