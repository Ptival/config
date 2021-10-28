{ src }:

import ../haskell-scaffolding.nix (rec {

  name = "llvm-pretty-bc-parser";
  inherit src;

  buildInputs = { pkgs, ... }:
    [
      pkgs.csmith
      pkgs.llvm_12
      pkgs.nodejs_latest
    ];

  modules = info: [
    {
      enableExecutableProfiling = true;
      enableLibraryProfiling = true;
    }
  ];

  packages = pkgs:
    [
      pkgs.${name}
    ];

})
