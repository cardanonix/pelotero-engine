#!/usr/bin/env bash
#
# scripts/mlb-api-sample.sh
#
# Fetch one item per endpoint from the live MLB statsapi.mlb.com so we can
# see the real response shapes. Output goes to stdout; redirect if you want
# to keep it.
#
# Env vars (all optional):
#   SEASON      default 2025
#   START_DATE  default 2025-07-15  (must be in-season for the boxscore step)
#   END_DATE    default same as START_DATE
#
# Requires: curl, jq

set -euo pipefail

SEASON="${SEASON:-2025}"
START_DATE="${START_DATE:-2025-07-15}"
END_DATE="${END_DATE:-${START_DATE}}"
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

section() {
  printf '\n=================================================================\n'
  printf '%s\n' "$1"
  printf '=================================================================\n'
}


# --- Teams ----------------------------------------------------------------
TEAMS_URL="${BASE}/api/v1/teams?sportId=1&season=${SEASON}"
section "TEAMS    GET ${TEAMS_URL}    (1 of teams[])"
"${CURL[@]}" "$TEAMS_URL" | jq '.teams |= .[0:1]'


# --- Players (active rosters) --------------------------------------------
PLAYERS_URL="${BASE}/api/v1/sports/1/players?activeStatus=ACTIVE&season=${SEASON}"
section "PLAYERS  GET ${PLAYERS_URL}    (1 of people[])"
"${CURL[@]}" "$PLAYERS_URL" | jq '.people |= .[0:1]'


# --- Schedule ------------------------------------------------------------
SCHED_URL="${BASE}/api/v1/schedule/games/?language=en&sportId=1&startDate=${START_DATE}&endDate=${END_DATE}"
section "SCHEDULE GET ${SCHED_URL}    (1 date, 1 game of that date)"
SCHED_JSON=$("${CURL[@]}" "$SCHED_URL")
echo "$SCHED_JSON" | jq '
  if (.dates | length) == 0
  then .
  else .dates = [ .dates[0] | .games |= .[0:1] ]
  end
'


# --- Boxscore (pulls gamePk from the schedule above) ---------------------
GAME_PK=$(echo "$SCHED_JSON" | jq -r '.dates[0].games[0].gamePk // empty')
if [ -n "$GAME_PK" ]; then
  BOX_URL="${BASE}/api/v1/game/${GAME_PK}/boxscore"
  section "BOXSCORE GET ${BOX_URL}    (1 away + 1 home player; rest of structure kept)"
  "${CURL[@]}" "$BOX_URL" | jq '
      .teams.away.players |= (to_entries | .[0:1] | from_entries)
    | .teams.home.players |= (to_entries | .[0:1] | from_entries)
  '
else
  section "BOXSCORE skipped (no game found for ${START_DATE}..${END_DATE})"
fi