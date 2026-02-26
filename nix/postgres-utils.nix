{ pkgs
, lib ? pkgs.lib
, name
, database ? null
}:

let
  pgConfig = if database != null then
    { database = database; }
  else
    import ./config.nix { inherit name; };

  postgresql = pkgs.postgresql;
  bin = {
    pgctl = "${postgresql}/bin/pg_ctl";
    psql = "${postgresql}/bin/psql";
    initdb = "${postgresql}/bin/initdb";
    createdb = "${postgresql}/bin/createdb";
    pgIsReady = "${postgresql}/bin/pg_isready";
  };

  config = {
    dataDir = pgConfig.database.dataDir;
    port = pgConfig.database.port;
    user = pgConfig.database.user;
    password = pgConfig.database.password;
  };

  settings = pgConfig.database.settings or { };

  listenAddresses = settings.listen_addresses or "localhost";
  maxConnections = settings.max_connections or 100;
  sharedBuffers = settings.shared_buffers or "128MB";
  dynamicSharedMemoryType = settings.dynamic_shared_memory_type or "posix";
  logDestination = settings.log_destination or "stderr";
  logDirectory = settings.log_directory or "log";
  logFilename = settings.log_filename or "postgresql-%Y-%m-%d_%H%M%S.log";

  mkPgConfig = ''
    listen_addresses = '${listenAddresses}'
    port = ${toString config.port}
    unix_socket_directories = '$PGDATA'
    max_connections = ${toString maxConnections}
    shared_buffers = '${sharedBuffers}'
    dynamic_shared_memory_type = '${dynamicSharedMemoryType}'
    log_destination = '${logDestination}'
    logging_collector = on
    log_directory = '${logDirectory}'
    log_filename = '${logFilename}'
  '';

  mkHbaConfig = ''
    local   all             all                                     trust
    host    all             all             127.0.0.1/32           trust
    host    all             all             ::1/128                trust
  '';

  envSetup = ''
    export PGPORT="''${PGPORT:-${toString config.port}}"
    export PGUSER="''${PGUSER:-${config.user}}"
    export PGDATABASE="''${PGDATABASE:-${pgConfig.database.name}}"
    export PGHOST="$PGDATA"
  '';

  validateEnv = ''
    if [ -z "$PGDATA" ]; then
      echo "Error: PGDATA environment variable must be set"
      exit 1
    fi
  '';

in {
  inherit config;

  pg-cleanup = pkgs.writeShellScriptBin "pg-cleanup" ''
    ${envSetup}
    ${validateEnv}

    echo "Checking for existing PostgreSQL processes on port $PGPORT..."
    EXISTING_PID=$(lsof -i :$PGPORT -t || true)

    if [ ! -z "$EXISTING_PID" ]; then
      echo "Found PostgreSQL process ($EXISTING_PID) using port $PGPORT"
      echo "Stopping process..."
      kill $EXISTING_PID || true

      RETRIES=0
      while kill -0 $EXISTING_PID 2>/dev/null; do
        RETRIES=$((RETRIES+1))
        if [ $RETRIES -eq 10 ]; then
          echo "Process not responding, forcing shutdown..."
          kill -9 $EXISTING_PID
          break
        fi
        sleep 1
      done
    fi

    if [ -d "$PGDATA" ]; then
      echo "Removing PGDATA directory..."
      rm -rf "$PGDATA"
    fi
  '';

  pg-start = pkgs.writeShellScriptBin "pg-start" ''
    ${envSetup}
    ${validateEnv}

    ${bin.pgctl} -D "$PGDATA" stop -m fast 2>/dev/null || true

    REAL_PGDATA=$(echo ${config.dataDir} | envsubst)
    mkdir -p "$REAL_PGDATA"
    mkdir -p "$PGDATA"

    echo "Initializing with user: $(whoami)"
    ${bin.initdb} -D "$PGDATA" \
        --auth=trust \
        --no-locale \
        --encoding=UTF8 \
        --username="$(whoami)"

    cat > "$PGDATA/postgresql.conf" << EOF
${mkPgConfig}
EOF

    cat > "$PGDATA/pg_hba.conf" << EOF
${mkHbaConfig}
EOF

    chown -R $(whoami) "$PGDATA"

    echo "Starting PostgreSQL..."
    ${bin.pgctl} -D "$PGDATA" -l "$PGDATA/postgresql.log" start

    if [ $? -ne 0 ]; then
      echo "PostgreSQL failed to start. Here's the log:"
      cat "$PGDATA/postgresql.log"
      exit 1
    fi

    echo "Waiting for PostgreSQL to be ready..."
    RETRIES=0
    while ! ${bin.pgIsReady} -h "$PGHOST" -p "$PGPORT" -q; do
      RETRIES=$((RETRIES+1))
      if [ $RETRIES -eq 10 ]; then
        echo "PostgreSQL failed to become ready. Here's the log:"
        cat "$PGDATA/postgresql.log"
        exit 1
      fi
      sleep 1
      echo "Still waiting... (attempt $RETRIES/10)"
    done

    echo "Creating database and user..."
    ${bin.psql} -h "$PGHOST" -p "$PGPORT" postgres << EOF
    DO \$\$
    BEGIN
      IF NOT EXISTS (SELECT FROM pg_user WHERE usename = '$(whoami)') THEN
        CREATE USER "$(whoami)" WITH PASSWORD '${config.password}' SUPERUSER;
      END IF;
    END
    \$\$;

    SELECT 'CREATE DATABASE ${pgConfig.database.name}'
    WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '${pgConfig.database.name}')\gexec

    GRANT ALL PRIVILEGES ON DATABASE ${pgConfig.database.name} TO "$(whoami)";
EOF

    echo "PostgreSQL is ready at: postgresql://$(whoami):${config.password}@localhost:$PGPORT/${pgConfig.database.name}"
  '';

  pg-connect = pkgs.writeShellScriptBin "pg-connect" ''
    ${envSetup}
    ${validateEnv}
    ${bin.psql} -h "$PGHOST" -p "$PGPORT" "$PGDATABASE"
  '';

  pg-stop = pkgs.writeShellScriptBin "pg-stop" ''
    ${envSetup}
    ${validateEnv}
    ${bin.pgctl} -D "$PGDATA" stop -m fast
  '';

  pg-backup = pkgs.writeShellScriptBin "pg-backup" ''
    ${envSetup}
    ${validateEnv}

    BACKUP_DIR="$HOME/.local/share/${name}/backups"
    mkdir -p "$BACKUP_DIR"
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    BACKUP_FILE="$BACKUP_DIR/${pgConfig.database.name}_$TIMESTAMP.sql"

    echo "Creating backup at $BACKUP_FILE..."
    ${postgresql}/bin/pg_dump -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" "$PGDATABASE" > "$BACKUP_FILE"

    if [ $? -eq 0 ]; then
      echo "Backup created successfully: $BACKUP_FILE"
    else
      echo "Backup failed"
      exit 1
    fi
  '';

  pg-restore = pkgs.writeShellScriptBin "pg-restore" ''
    ${envSetup}
    ${validateEnv}

    if [ -z "$1" ]; then
      echo "Usage: pg-restore <backup-file>"
      echo "Available backups:"
      ls -lt "$HOME/.local/share/${name}/backups" 2>/dev/null || echo "No backups found"
      exit 1
    fi

    if [ ! -f "$1" ]; then
      echo "Backup file not found: $1"
      exit 1
    fi

    echo "Restoring from $1..."
    ${postgresql}/bin/psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" "$PGDATABASE" < "$1"
  '';

  pg-stats = pkgs.writeShellScriptBin "pg-stats" ''
    ${envSetup}
    ${validateEnv}

    echo "Database Statistics for ${pgConfig.database.name}"
    echo "==============================="

    ${bin.psql} -h "$PGHOST" -p "$PGPORT" "$PGDATABASE" << EOF
      \echo 'Database Size:'
      SELECT pg_size_pretty(pg_database_size('$PGDATABASE'));

      \echo '\nConnection Count:'
      SELECT count(*) FROM pg_stat_activity;

      \echo '\nTable Sizes:'
      SELECT relname as table_name,
             pg_size_pretty(pg_total_relation_size(relid)) as total_size,
             n_live_tup as row_count
      FROM pg_stat_user_tables
      ORDER BY pg_total_relation_size(relid) DESC;
EOF
  '';

  pg-rotate-credentials = pkgs.writeShellScriptBin "pg-rotate-credentials" ''
    ${envSetup}
    ${validateEnv}

    NEW_PASSWORD=$(${pkgs.openssl}/bin/openssl rand -base64 12)

    echo "Rotating password for user $PGUSER..."
    ${bin.psql} -h "$PGHOST" -p "$PGPORT" postgres -c \
      "ALTER USER \"$PGUSER\" WITH PASSWORD '$NEW_PASSWORD';"

    if [ $? -eq 0 ]; then
      echo "Password rotated successfully"
      echo "New password: $NEW_PASSWORD"
    else
      echo "Password rotation failed"
      exit 1
    fi
  '';
}