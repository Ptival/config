{ src }:

import ../haskell-scaffolding.nix (rec {

  compiler-nix-name = "ghc8104";
  name = "chick";
  inherit src;
  subDir = "backend";

  buildInputs = { set, ... }:
    [
      set.happy.components.exes.happy
    ];

  packages = p: [
    p.${name}
    p.language-ocaml
  ];

})
