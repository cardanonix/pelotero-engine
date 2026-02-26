{ pkgs, lib ? pkgs.lib, name }:

let
  config = import ./config.nix { inherit name; };

  host = config.network.host;
  bindAddress = config.network.bindAddress;
  backendPort = toString config.haskell.port;
  dbPort = toString config.database.port;
  dataDir = config.dataDir;

  db-start = pkgs.writeShellScriptBin "db-start" ''
    set -euo pipefail

    echo "Starting database service on port ${dbPort}..."

    BACKUP_DIR="${dataDir}/backups"
    mkdir -p "$BACKUP_DIR"
    LATEST_BACKUP="$(find "$BACKUP_DIR" -type f -name '*.sql' -printf '%T@ %p\n' 2>/dev/null | sort -nr | head -n1 | cut -d' ' -f2- || true)"

    if [ -z "$LATEST_BACKUP" ]; then
      echo "No backup found, starting fresh database..."
      pg-start
    else
      echo "Found backup at $LATEST_BACKUP"
      pg-start
      echo "Restoring from backup..."
      pg-restore "$LATEST_BACKUP"
    fi

    echo "Database started at ${host}:${dbPort}"
    watch -n 5 pg-stats
  '';

  db-stop = pkgs.writeShellScriptBin "db-stop" ''
    set -euo pipefail
    echo "Creating database backup..."
    pg-backup
    echo "Stopping database..."
    pg-stop || { echo "Failed to stop PostgreSQL"; exit 1; }
    echo "Database stopped."
  '';

  backend-start = pkgs.writeShellScriptBin "backend-start" ''
    set -euo pipefail
    echo "Building and starting backend..."
    cabal build || { echo "Build failed"; exit 1; }
    echo "Starting backend on ${host}:${backendPort}..."
    exec cabal run cheeblr-backend 2>&1 || exec cabal run fetch-rosters 2>&1
  '';

  # Run the new fetch-rosters executable
  fetch-rosters = pkgs.writeShellScriptBin "fetch-rosters" ''
    set -euo pipefail
    SEASON="''${1:-2025}"
    echo "Fetching rosters for season $SEASON..."
    cabal run fetch-rosters -- "$SEASON"
  '';

  # Dev mode: database + watch for changes
  dev = pkgs.writeShellScriptBin "pe-dev" ''
    set -euo pipefail

    echo "Starting pelotero-engine dev environment..."

    # Start postgres if not running
    if ! pg_isready -h "$PGHOST" -p "$PGPORT" -q 2>/dev/null; then
      echo "Starting PostgreSQL..."
      pg-start

      BACKUP_DIR="${dataDir}/backups"
      LATEST_BACKUP="$(find "$BACKUP_DIR" -type f -name '*.sql' -printf '%T@ %p\n' 2>/dev/null | sort -nr | head -n1 | cut -d' ' -f2- || true)"
      if [ -n "$LATEST_BACKUP" ]; then
        echo "Restoring from backup: $LATEST_BACKUP"
        pg-restore "$LATEST_BACKUP"
      fi
    else
      echo "PostgreSQL already running."
    fi

    echo ""
    echo "Database ready at: postgresql://$(whoami)@localhost:$PGPORT/fantasy_league"
    echo ""
    echo "Available commands:"
    echo "  cabal build                    - Build everything"
    echo "  cabal run fetch-rosters -- 2025 - Fetch MLB rosters"
    echo "  pg-connect                     - psql into fantasy_league"
    echo "  pg-stats                       - Database statistics"
    echo "  pg-backup                      - Backup database"
    echo "  pg-stop                        - Stop PostgreSQL"
    echo ""
  '';

  deploy = pkgs.writeShellScriptBin "pe-deploy" ''
    set -euo pipefail

    echo "TMux Commands:"
    echo "  Ctrl-b d    - Detach"
    echo "  Ctrl-b o    - Switch panes"
    echo ""
    echo "Starting services..."
    echo "  Backend:  http://${host}:${backendPort}"
    echo "  Postgres: ${host}:${dbPort}"
    echo ""

    tmux kill-session -t ${name} 2>/dev/null || true
    tmux new-session -d -s ${name} -n "Services" -x 120 -y 42

    # Two panes: db stats on top, shell on bottom
    tmux split-window -v -b -l 12

    tmux send-keys -t ${name}:Services.0 'watch -n 5 pg-stats' C-m
    tmux send-keys -t ${name}:Services.1 'pe-dev' C-m

    tmux select-pane -t ${name}:Services.1
    tmux attach-session -t ${name}
  '';

  stop = pkgs.writeShellScriptBin "pe-stop" ''
    set -euo pipefail

    echo "Creating database backup..."
    pg-backup || true

    echo "Stopping database..."
    pg-stop || true

    echo "Stopping tmux session..."
    tmux kill-session -t ${name} 2>/dev/null || true

    echo "All services stopped."
  '';

in {
  inherit db-start db-stop backend-start fetch-rosters dev deploy stop;
}