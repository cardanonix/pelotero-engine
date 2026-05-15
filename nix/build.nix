{ inputs }:

let
  inherit (inputs) nixpkgs flake-utils haskellNix iohkNix CHaP hackage;

  appConfig = import ./config.nix { };
  name      = appConfig.name;

  mkSystemOutputs = system:
    let
      lib = nixpkgs.lib;

      pkgs = import haskellNix.inputs.nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [
          haskellNix.overlay
          iohkNix.overlays.crypto
        ];
      };

      haskellProject = pkgs.haskell-nix.project' {
        src = ../.;
        compiler-nix-name = "ghc910";

        inputMap = {
          "https://chap.intersectmbo.org/" = CHaP;
          "https://hackage.haskell.org/"   = hackage;
        };

        shell = {
          tools = {
            cabal = { };
            haskell-language-server = { };
            hlint = { };
            fourmolu = { };
          };

          buildInputs = with pkgs; [
            pkg-config
            postgresql.lib
            openssl.dev
            zlib
          ];
        };

        modules = [{
          packages.http-client-tls.postPatch = ''
            substituteInPlace http-client-tls.cabal --replace-warn "memory" "ram"
          '';
        }];
      };

      backendFlake = haskellProject.flake { };

      dbConfig = appConfig.database;

      postgresModule = import ./postgres-utils.nix {
        inherit pkgs name;
        database = dbConfig;
      };

      deployModule = import ./deploy.nix {
        inherit pkgs name;
      };

      sopsModule = import ./sops-dev.nix {
        inherit pkgs lib name;
      };

      fileTools = import ./scripts/file-tools.nix {
        inherit pkgs lib name;
        backendPath = ".";
        hsDirs      = [ "lib" "src-new" "app" ];
        hsTestDirs  = [];
        hsConfig    = appConfig.haskell;
      };

      # Single canonical executable. If this attr is missing it means the
      # cabal file no longer defines `executable pelotero`, which is a real
      # build problem rather than something to paper over with a fallback.
      defaultPackage = backendFlake.packages."${name}:exe:pelotero";

    in {
      legacyPackages = pkgs;

      packages = backendFlake.packages // {
        default = defaultPackage;
      };

      devShells = let
        shell = pkgs.mkShell {
          inherit name;

          inputsFrom = [ backendFlake.devShells.default ];

          buildInputs = with pkgs; [
            postgresModule.pg-start
            postgresModule.pg-connect
            postgresModule.pg-stop
            postgresModule.pg-cleanup
            postgresModule.pg-backup
            postgresModule.pg-restore
            postgresModule.pg-rotate-credentials
            postgresModule.pg-stats

            deployModule.db-start
            deployModule.db-stop
            deployModule.dev
            deployModule.deploy
            deployModule.stop

            sopsModule.sops-init-key
            sopsModule.sops-pubkey
            sopsModule.sops-bootstrap
            sopsModule.sops-get
            sopsModule.sops-exec
            sopsModule.sops-status

            fileTools.generate-manifest
            fileTools.compile-manifest
            fileTools.compile-archive
            fileTools.llm-context
            fileTools.manifest-tui

            postgresql
            pgcli
            pkg-config
            openssl.dev
            zlib
            lsof
            tmux
            gettext
            jq
            sops
            age
            ssh-to-age
            gum
          ];

          shellHook = ''
            export PGDATA="${dbConfig.dataDir}"
            export PGPORT="${toString dbConfig.port}"
            export PGUSER="${dbConfig.user}"
            export PGDATABASE="${dbConfig.name}"
            export PGHOST="$PGDATA"
            export PKG_CONFIG_PATH="${pkgs.postgresql.lib}/lib/pkgconfig:$PKG_CONFIG_PATH"
            export SOPS_AGE_KEY_FILE="$HOME/.config/sops/age/pelotero-engine.txt"

            mkdir -p "$(pwd)/script/concat_archive/output" \
                     "$(pwd)/script/concat_archive/archive" \
                     "$(pwd)/script/concat_archive/.hashes"

            echo ""
            echo "  Pelotero Engine Dev Environment"
            echo "  ================================"
            echo ""
            echo "  Secrets (sops):"
            ${sopsModule.loadSecretsHook}
            echo ""
            echo "  Database (port ${toString dbConfig.port}):"
            echo "    pg-start               Start PostgreSQL"
            echo "    pg-connect             Connect via psql"
            echo "    pg-stop                Stop PostgreSQL"
            echo "    pg-cleanup             Remove data directory"
            echo "    pg-backup              Backup database"
            echo "    pg-restore <file>      Restore from backup"
            echo "    pg-rotate-credentials  Rotate DB password"
            echo "    pg-stats               Show DB statistics"
            echo ""
            echo "  Development:"
            echo "    pe-dev                 Ensure DB is up, print connection info"
            echo "    pe-deploy              tmux session with DB stats + dev panes"
            echo "    pe-stop                Backup, stop DB, kill tmux session"
            echo ""
            echo "  Build / run:"
            echo "    cabal build all        Build library + executable + tests"
            echo "    cabal run pelotero     Run the CLI (try '-- --help')"
            echo "    cabal test             Run unit + property tests"
            echo "    ./tui                  Interactive build/run/test/db menu"
            echo "    ./tui --build-all      Non-interactive: nix-build all executables"
            echo ""
            echo "  LLM context:"
            echo "    generate-manifest      Scan source -> script/manifest.json"
            echo "    compile-manifest       Bundle source files for review"
            echo "    llm-context            Generate context from git diff"
            echo "    manifest-tui           Interactive TUI for the above"
            echo ""
          '';
        };
      in {
        default = shell;
      };
    };

in {
  perSystem = mkSystemOutputs;
  systems = [
    "x86_64-linux"
    "aarch64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];
}