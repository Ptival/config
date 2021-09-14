{ src }:

import ../haskell-scaffolding.nix (rec {

  name = "llvm-pretty-bc-parser";
  inherit src;

  buildInputs = { pkgs, ... }:
    [
      pkgs.nodejs_latest
    ];

  packages = pkgs:
    [
      pkgs.${name}
    ];

})
