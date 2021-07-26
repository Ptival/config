{ src }:

import ../haskell-scaffolding.nix (rec {

  compiler-nix-name = "ghc8104";
  name = "reanimate-3b1b-morphisms";
  inherit src;

  packages = pkgs:
    [
      pkgs.${name}
    ];

})
