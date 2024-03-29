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
      pkgs.ghidra
      llvmPackages.libstdcxxClang
      # pkgs.lean
      llvmPackages.llvm
      # pkgs.yices
      # pkgs.z3
    ];

  modules = { ... }: [
    {
      enableLibraryProfiling = true;
    }
  ];

  packages = pkgs:
    [
      pkgs.reopt
      # The following are listed so that their output shows up in Hoogle:
      pkgs.macaw-x86 # Actually this does not work...
    ];

  sourceFilter = { filters, ... }: filters.nodeModulesFilter;

})
