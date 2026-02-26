{ pkgs, name, lib, hsDirs, hsConfig ? { } }:

let
  manifestModule = import ./manifest.nix {
    inherit pkgs lib;
    config = {
      inherit hsDirs;
      inherit hsConfig;
    };
  };

  devScriptsModule = import ./devScripts.nix {
    inherit pkgs name lib hsDirs hsConfig;
  };

in {
  # Scripts
  inherit (devScriptsModule) compile-manifest compile-archive;
  generate-manifest = manifestModule.generateScript;

  # All tools as a list (for easy inclusion in buildInputs)
  tools = [
    devScriptsModule.compile-manifest
    devScriptsModule.compile-archive
    manifestModule.generateScript
  ];

  # Debug info
  debug = manifestModule.debug;

  # Resolved config
  config = {
    inherit hsDirs;
  };
}