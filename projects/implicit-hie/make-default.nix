{ src }:

import ../haskell-scaffolding.nix (rec {

  compiler-nix-name = "ghc8104";
  name = "implicit-hie";
  inherit src;

  packages = pkgs:
    [
      pkgs.${name}
    ];

})
