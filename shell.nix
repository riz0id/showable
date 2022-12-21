{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.mkShell {
  buildInputs = (with pkgs; [
    hlint
    haskell-language-server
  ]);
}