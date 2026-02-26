{
  description = "Pelotero Hix/Pix/Plutus dApp DevEnv";

  inputs = {
    # iogx = {
    #   url = "github:input-output-hk/iogx";
    #   inputs.hackage.follows = "hackage";
    #   inputs.CHaP.follows = "CHaP";
    #   inputs.haskell-nix.follows = "haskellNix";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage";
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?rev=35d5d7f7e7cfed87901623262ceea848239fa7f8";
      flake = false;
    };

    plutus.url = "github:IntersectMBO/plutus";

    styleguide.url = "github:cardanonix/styleguide";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP, plutus, styleguide, ... }:
    let
    
      overlays = [
        haskellNix.overlay
      ];

      back_EndResults = flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (
        system: let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          inherit styleguide;
          # hixProject = pkgs.haskell-nix.hix.project {
          #   src = ./.;
          #   evalSystem = system;
          #   inputMap = {"https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;};
          #   # NOTE: cardano-crypto overrides commented out until those deps
          #   # are back in the plan. Uncomment when adding plutus/cardano deps.
          #   # modules = [
          #   #   (_: {
          #   #     packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf];
          #   #     packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf pkgs.secp256k1];
          #   #   })
          #   # ];
          #   modules = [];
          # };
          # hixFlake = hixProject.flake {};



          # ── Config + Postgres + Deploy + Dev Scripts ────────────
          appConfig = import ./nix/config.nix { name = "pelotero-engine"; };
          dbConfig = appConfig.database;

          postgresModule = import ./nix/postgres-utils.nix {
            inherit pkgs;
            name = "pelotero-engine";
            database = appConfig.database;
          };

          deployModule = import ./nix/deploy.nix {
            inherit pkgs;
            name = "pelotero-engine";
          };

          devScripts = import ./nix/scripts/file-tools.nix {
            inherit pkgs;
            lib = pkgs.lib;
            name = "pelotero-engine";
            hsDirs = [ "src" "src-new" "app" ];
          };
          # ── End imports ─────────────────────────────────────────

        in {
          # apps = hixFlake.apps;
          # checks = hixFlake.checks;
          # checks.format = styleguide.lib.${system}.mkCheck self;
          # formatter = styleguide.lib.${system}.mkFormatter self;
          # packages = hixFlake.packages;

          legacyPackages = pkgs;

          devShell = pkgs.mkShell {
            name = "pelotero-engine";
            # inputsFrom = [hixFlake.devShells.default];
            buildInputs = [
              (pkgs.haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
              ]))
              pkgs.zlib

              # ── PostgreSQL management ───────────────────────────
              postgresModule.pg-start
              postgresModule.pg-connect
              postgresModule.pg-stop
              postgresModule.pg-cleanup
              postgresModule.pg-backup
              postgresModule.pg-restore
              postgresModule.pg-rotate-credentials
              postgresModule.pg-stats

              # ── Dev/deploy scripts ──────────────────────────────
              deployModule.db-start
              deployModule.db-stop
              deployModule.fetch-rosters
              deployModule.dev
              deployModule.deploy
              deployModule.stop

              # ── File processing / manifest scripts ──────────────
              devScripts.generate-manifest
              devScripts.compile-manifest
              devScripts.compile-archive
              # ── End build inputs ────────────────────────────────
            ];
            packages = with pkgs; [
              haskellPackages.fourmolu
              zlib
              nix-tree
              cabal-install

              # ── Packages for database + scripts ─────────────────
              postgresql
              postgresql.lib
              pgcli
              pkg-config
              openssl.dev
              lsof
              tmux
              gettext
              jq
              perl
              findutils
              coreutils
              gnused
              gnugrep
              # ── End packages ────────────────────────────────────
            ];
            shellHook = ''
              # ── Database environment variables ──────────────────
              export PGDATA="${dbConfig.dataDir}"
              export PGPORT="${toString dbConfig.port}"
              export PGUSER="${dbConfig.user}"
              export PGPASSWORD="${dbConfig.password}"
              export PGDATABASE="${appConfig.database.name}"
              export PGHOST="$PGDATA"
              export PKG_CONFIG_PATH="${pkgs.postgresql.lib}/lib/pkgconfig:$PKG_CONFIG_PATH"

              # ── Ensure script directories exist ─────────────────
              mkdir -p "$(pwd)/script/concat_archive/output" \
                       "$(pwd)/script/concat_archive/archive" \
                       "$(pwd)/script/concat_archive/.hashes"

              echo ""
              echo "  Pelotero Engine Dev Environment"
              echo "  ================================"
              echo ""
              echo "  Database:"
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
              echo "    pe-dev                 Start DB + dev shell"
              echo "    pe-deploy              Deploy with tmux"
              echo "    pe-stop                Stop everything"
              echo "    fetch-rosters [year]   Fetch MLB rosters (default: 2025)"
              echo ""
              echo "  File Processing:"
              echo "    generate-manifest      Scan and generate manifest.json"
              echo "    compile-manifest       Compile Haskell + Nix per manifest"
              echo "    compile-archive        Archive ALL files (ignores manifest)"
              echo ""
              echo "  Build:"
              echo "    cabal build            Build all targets"
              echo "    cabal run fetch-rosters -- 2025"
              echo ""
            '';
          };
        }
      );
    in
      back_EndResults
      // {
        # packages = back_EndResults.packages;
        devShell = back_EndResults.devShell;
      };

  nixConfig = {
    extra-experimental-features = ["nix-command flakes" "ca-derivations"];
    allow-import-from-derivation = "true";
    extra-substituters = [
      "https://klarkc.cachix.org?priority=99"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
  };
}