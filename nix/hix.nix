{pkgs, ...}: {
  name = "pelotero_engine";
  compiler-nix-name = "ghc966";
  shell.tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}