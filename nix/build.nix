{ inputs }:

let
  inherit (inputs) nixpkgs flake-utils haskellNix iohkNix CHaP;

  name = "pelotero-engine";

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
        compiler-nix-name = "ghc928";

        inputMap = {
          "https://chap.intersectmbo.org/" = CHaP;
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
          packages.postgresql-libpq.flags.use-pkg-config = true;
          packages.postgresql-simple.flags.use-pkg-config = true;
        }];
      };

      backendFlake = haskellProject.flake { };

      postgresModule = import ./postgres-utils.nix {
        inherit pkgs name;
        database = (import ./config.nix { inherit name; }).database;
      };

      deployModule = import ./deploy.nix {
        inherit pkgs name;
      };

      appConfig = import ./config.nix { inherit name; };
      dbConfig = appConfig.database;

    in {
      legacyPackages = pkgs;

      packages = backendFlake.packages // {
        default = backendFlake.packages."${name}:exe:fetch-rosters" or
                  backendFlake.packages."${name}:lib:${name}" or
                  (builtins.head (builtins.attrValues backendFlake.packages));
      };

      devShells = let
        shell = pkgs.mkShell {
          inherit name;

          inputsFrom = [
            backendFlake.devShells.default
          ];

          buildInputs = with pkgs; [
            # PostgreSQL management
            postgresModule.pg-start
            postgresModule.pg-connect
            postgresModule.pg-stop
            postgresModule.pg-cleanup
            postgresModule.pg-backup
            postgresModule.pg-restore
            postgresModule.pg-rotate-credentials
            postgresModule.pg-stats

            # Deploy/dev scripts
            deployModule.db-start
            deployModule.db-stop
            deployModule.backend-start
            deployModule.fetch-rosters
            deployModule.dev
            deployModule.deploy
            deployModule.stop

            # System tools
            postgresql
            pgcli
            pkg-config
            openssl.dev
            zlib
            lsof
            tmux
            gettext  # for envsubst
            jq
          ];

          shellHook = ''
            export PGDATA="${dbConfig.dataDir}"
            export PGPORT="${toString dbConfig.port}"
            export PGUSER="${dbConfig.user}"
            export PGPASSWORD="${dbConfig.password}"
            export PGDATABASE="${appConfig.database.name}"
            export PGHOST="$PGDATA"
            export PKG_CONFIG_PATH="${pkgs.postgresql.lib}/lib/pkgconfig:$PKG_CONFIG_PATH"

            echo ""
            echo "  ╔══════════════════════════════════════╗"
            echo "  ║     Pelotero Engine Dev Environment   ║"
            echo "  ╚══════════════════════════════════════╝"
            echo ""
            echo "  Database:"
            echo "    pg-start          Start PostgreSQL"
            echo "    pg-connect        Connect via psql"
            echo "    pg-stop           Stop PostgreSQL"
            echo "    pg-backup         Backup database"
            echo "    pg-restore <f>    Restore from backup"
            echo "    pg-stats          Show DB statistics"
            echo ""
            echo "  Development:"
            echo "    pe-dev            Start dev environment (DB + shell)"
            echo "    pe-deploy         Deploy with tmux"
            echo "    pe-stop           Stop everything"
            echo "    fetch-rosters     Fetch MLB rosters (default: 2025)"
            echo ""
            echo "  Build:"
            echo "    cabal build       Build all targets"
            echo "    cabal run fetch-rosters -- 2025"
            echo ""
          '';
        };
      in {
        default = shell;
      };

      devShell = shell;
    };

in {
  perSystem = mkSystemOutputs;
  systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
}