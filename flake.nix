{
  # Haskell Hix dApp Dev Environment
  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";

    #Haskell/Plutus
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    plutus.url = "github:input-output-hk/plutus";
  };

  outputs = {
    self,
    utils,
    ...
  } @ inputs:
    utils.apply-systems
    {
      inherit inputs;
      systems = ["x86_64-linux"];
      overlays = [
        inputs.haskell-nix.overlay
        # plutus runtime dependency
        inputs.iohk-nix.overlays.crypto
      ];
    }
    ({
        pkgs,
        system,
        ...
      } @ context: let
        hixProject = pkgs.haskell-nix.hix.project {
          src = ./.;
          evalSystem = "x86_64-linux";
          inputMap = {"https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;};
          modules = [
            (_: {
              # See input-output-hk/iohk-nix#488
              packages.cardano-crypto-praos.components.library.pkgconfig =
                pkgs.lib.mkForce [[pkgs.libsodium-vrf]];
              packages.cardano-crypto-class.components.library.pkgconfig =
                pkgs.lib.mkForce [[pkgs.libsodium-vrf pkgs.secp256k1]];
            })
          ];
        };
        hixFlake = hixProject.flake {};
        # serve-docs = import ./nix/serve-docs.nix inputs context {
        #   inherit hixProject;
        #   # TODO transform additionalPkgs in excludePkgs to reduce boilerplate
        #   #  we could collect all entries from cabal build-depends
        #   #  (maybe through hixProject.hsPkgs)
        #   additionalPkgs = ["cardano-api"];
        # };
      in
        /*
           # Flake definition follows hello.cabal
        flake
        // {
          legacyPackages = pkgs;
        });
        */
        {
          inherit (hixFlake) apps checks;
          legacyPackages = pkgs;

          packages =
            hixFlake.packages
            // {
              # inherit serve-docs;
            };

          devShell = pkgs.mkShell {
            inputsFrom = [
              hixFlake.devShell
            ];
            buildInputs = [
              # self.packages.${system}.serve-docs
            ];
            shellHook = ''
              export NIX_SHELL_NAME="scraperProto"
              echo "Welcome to the development shell!"
              echo
              echo grabbing the Current Complete MLB roster right quick....
              cabal build
              cabal run scraperProto 2021-08-22 2021-08-23
              echo .
              echo ..
              echo ...
            '';
          };
        });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
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
