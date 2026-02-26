{ pkgs, name, lib, hsDirs, hsConfig ? { } }:

let
  manifestModule = import ./manifest.nix {
    inherit pkgs lib;
    config = {
      hsDirs = hsDirs;
      hsConfig = hsConfig;
    };
  };

  devScriptsModule = import ./devScripts.nix {
    inherit pkgs name lib hsDirs hsConfig;
  };

in {
  inherit (devScriptsModule) compile-manifest compile-archive;

  generate-manifest = manifestModule.generateScript;

  tools = [
    devScriptsModule.compile-manifest
    devScriptsModule.compile-archive
    manifestModule.generateScript
  ];

  debug = manifestModule.debug;
}