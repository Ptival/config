{ src }:

import ../haskell-scaffolding.nix (rec {

  compiler-nix-name = "ghc8104";
  name = "reopt";
  inherit src;

  sourceFilter = { pkgs, ... }: name: type:
    let baseName = baseNameOf (toString name);
    in
    pkgs.haskell-nix.haskellSourceFilter name type && !(
      # this trips haskell.nix as it contains files named package.yaml
      baseName == "node_modules"
      # || other conditions...
    );

  buildInputs = { pkgs, ... }: [
    pkgs.clang
    pkgs.llvm
    pkgs.yices
    pkgs.z3
  ];

  packages = pkgs:
    [
      pkgs.${name}
    ];

})
