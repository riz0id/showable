{ ghc ? "ghc942" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.mkShell {
  buildInputs = (with pkgs; [
    haskell-language-server
  ]);
}