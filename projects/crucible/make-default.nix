{ src }:

import ../haskell-scaffolding.nix (rec {

  name = "crucible";
  inherit src;

  # DYLD_INSERT_LIBRARIES="${workaround}/macos11ghcwa.dylib";

  buildInputs = { pkgs, ... }:
    [
      pkgs.boost # for crucible-wasm?
      pkgs.clang
      pkgs.glpk # for BLT
      # pkgs.haskellPackages.cabal-fmt
      pkgs.llvm
      pkgs.ntl # for BLT
      pkgs.websocat # for websocket debugging
      pkgs.yices
      pkgs.z3
    ];

  modules = info@{ macOSWorkaround, pkgs, ... }:
    let
      preConfigureWorkaround = ''
        export DYLD_INSERT_LIBRARIES=${macOSWorkaround}/macos11ghcwa.dylib
      '';
    in
    [
      {
        packages.abcBridge.components.library.build-tools = [
          pkgs.abc-verifier
        ];
        # packages.cryptol-saw-core.components.library.preConfigure = preConfigureWorkaround;
        # packages.saw-core-coq.components.library.preConfigure = preConfigureWorkaround;
        packages.wasm.components.library.preConfigure = preConfigureWorkaround;
      }
    ];

  overlays = { sources }:
    [
      # NOTE: abc-verifier in nixpkgs does not package its lib and include, just the exe
      # (self: super: { abc = self.abc-verifier; })
      (self: super: { abc = self.callPackage ../../nix-expressions/abc { source = sources.abc; }; })
    ];

  packages = pkgs:
    [
      pkgs.abcBridge # keep this for ABC library
      pkgs.crucible
      pkgs.crucible-concurrency
      pkgs.crucible-go
      pkgs.crucible-jvm
      pkgs.crucible-llvm
      pkgs.crucible-wasm
      pkgs.crucible-syntax
      pkgs.crux
      pkgs.crux-llvm
      pkgs.crux-mir
      # pkgs.flexdis86
      # pkgs.jvm-verifier
      # pkgs.llvm-pretty
      # pkgs.parameterized-utils
      # pkgs.saw-core
      pkgs.uc-crux-llvm
      pkgs.what4
    ];

  sourceFilter = { filters, ... }: filters.nodeModulesFilter;

})
