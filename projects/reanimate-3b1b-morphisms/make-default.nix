{ src }:

import ../haskell-scaffolding.nix (rec {

  compiler-nix-name = "ghc8104";
  name = "reanimate-3b1b-morphisms";
  inherit src;

  overlays = info: [
    (self: super: {
      m = self.openlibm;
    })
  ];

  packages = pkgs:
    [
      pkgs.${name}
    ];

})
