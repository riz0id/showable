{ ghc }: 

import (import ./nixpkgs.nix) {
  config.packageOverrides = (prev: {
    haskell = prev.haskell // {
      packages = prev.haskell.packages // {
        "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
          showable = self.callCabal2nix "showable" ../../. { };
        });
      };
    };
  });
}