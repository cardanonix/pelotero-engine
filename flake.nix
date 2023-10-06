{
  description = "Pelotero Hix/Pix/Plutus dApp DevEnv";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    plutus.url = "github:input-output-hk/plutus";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
    iohk-nix,
    CHaP,
    plutus,
  }: let
    overlays = [
      haskellNix.overlay
      iohk-nix.overlays.crypto
      (final: prev: {
        helloProject = final.haskell-nix.project' {
          src = ./.;
          compiler-nix-name = "ghc925";
          shell.tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };
      })
    ];
    # front_EndResults = flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (
    #   system: let
    #     overlays = [
    #       haskellNix.overlay
    #       iohk-nix.overlays.crypto
    #       (final: prev: {
    #         helloProject = final.haskell-nix.project' {
    #           src = ./.;
    #           compiler-nix-name = "ghc925";
    #           shell.tools = {
    #             cabal = "latest";
    #             hlint = "latest";
    #             haskell-language-server = "latest";
    #           };
    #         };
    #       })
    #     ];
    #     pkgs = import nixpkgs {
    #       inherit system overlays;
    #       inherit (haskellNix) config;
    #     };
    #     hixProject = pkgs.haskell-nix.hix.project {
    #       src = ./.;
    #       evalSystem = system;
    #       inputMap = {"https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;};
    #       modules = [
    #         (_: {
    #           packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf];
    #           packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf pkgs.secp256k1];
    #         })
    #       ];
    #     };
    #     hixFlake = hixProject.flake {};
    #   in {
    #     apps = hixFlake.apps;
    #     checks = hixFlake.checks;
    #     packages = hixFlake.packages;
    #     legacyPackages = pkgs;
    #     devShell = pkgs.mkShell {
    #       name = "frontEnd";
    #       inputsFrom = [hixFlake.devShell];
    #       buildInputs = [];
    #       packages = [];
    #       shellHook = ''
    #         export NIX_SHELL_NAME="scraper"
    #       '';
    #     };
    #   }
    # );
    back_EndResults = flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (
      system: let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        hixProject = pkgs.haskell-nix.hix.project {
          src = ./.;
          evalSystem = system;
          inputMap = {"https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;};
          modules = [
            (_: {
              packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf];
              packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf pkgs.secp256k1];
            })
          ];
        };

        hixFlake = hixProject.flake {};
      in {
        apps = hixFlake.apps;
        checks = hixFlake.checks;
        packages = hixFlake.packages;
        legacyPackages = pkgs;

        devShell = pkgs.mkShell {
          name = "scraper";
          inputsFrom = [hixFlake.devShell];
          buildInputs = [];
          packages = [
            pkgs.haskellPackages.hls-fourmolu-plugin
            pkgs.zlib
          ];
          # shellHook = ''
          #   export NIX_SHELL_NAME="scraper"
          #   echo "Welcome to the development shell!"
          #   echo properly populating your folders.

          #   if [ ! -d "appData" ]; then
          #       echo creating appData folder
          #       mkdir "appData"
          #   fi

          #   cd "appData"

          #   folders=("config" "rosters" "stats" "points")

          #   for folder in ''${folders[@]}; do
          #       if [ ! -d "$folder" ]; then
          #         echo creating $folder
          #         mkdir "$folder"
          #       fi
          #   done
          #   cd -
          #   echo Building the Apps...
          #   echo .
          #   echo ..
          #   echo ...
          #   cabal build
          #   cabal run roster
          #   echo .
          #   echo ..
          #   echo ...
          # '';
        };
      }
    );
    oci_ImageResult = flake-utils.lib.eachDefaultSystem (system: let
      overlayPkgs = import nixpkgs {
        inherit system overlays;
      };
      linuxPkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = overlays;
      };
    in {
      packages.dev-env-docker = overlayPkgs.dockerTools.buildImage {
        name = "dev-env-docker";
        tag = "0.1.0";

        # The packFunction ensures all packages are accessible inside the container.
        packFunction = overlayPkgs.callPackage ({
          writeScriptBin,
          bash,
          coreutils,
        }:
          writeScriptBin "setup-environment" ''
            #!${bash}/bin/bash
            export PATH="${coreutils}/bin:${overlayPkgs.haskellPackages.hls-fourmolu-plugin}/bin:${overlayPkgs.zlib}/bin:$PATH"
          '' {});

        contents = [
          overlayPkgs.haskellPackages.hls-fourmolu-plugin
          overlayPkgs.zlib
        ];

        config = {
          Cmd = ["setup-environment"];
        };

        created = "2023_10_06_10_22_00";
      };

      devShells.default = overlayPkgs.mkShell {
        buildInputs = with overlayPkgs; [bat vim];
      };
    });
  in {
    apps = back_EndResults.apps // oci_ImageResult.apps;
    checks = back_EndResults.checks // oci_ImageResult.checks;
    packages = back_EndResults.packages // oci_ImageResult.packages;
    legacyPackages = back_EndResults.legacyPackages;
    devShell = back_EndResults.devShell;
  };
  nixConfig = {
    extra-experimental-features = ["nix-command flakes" "ca-derivations"];
    allow-import-from-derivation = "true";
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
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
