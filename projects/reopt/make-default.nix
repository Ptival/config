{ src }:

let

  pickLLVM = { pkgs, ... }: pkgs.llvmPackages_12;

in

import ../haskell-scaffolding.nix (rec {

  name = "reopt";
  inherit src;

  buildInputs = { pkgs, ... }:
    let
      llvmPackages = pickLLVM pkgs;
    in
    [
      llvmPackages.clang
      # pkgs.elan
      # (pkgs.enableDebugging pkgs.gdb)
      llvmPackages.libstdcxxClang
      # pkgs.lean
      llvmPackages.llvm
      # pkgs.yices
      # pkgs.z3
    ];

  packages = pkgs:
    [
      pkgs.${name}
    ];

  sourceFilter = { filters, ... }: filters.nodeModulesFilter;

})
