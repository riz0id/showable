{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs) 
    mkShell;

  inherit (pkgs.haskell.packages."${ghc}") 
    showable
    hlint
    haskell-language-server;
}
