{ src }:

import ../haskell-scaffolding.nix (rec {

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
