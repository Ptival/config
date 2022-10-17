{ src }:

import ../haskell-scaffolding.nix
  (rec {

    name = "macaw";
    inherit src;

    # DYLD_INSERT_LIBRARIES="${workaround}/macos11ghcwa.dylib";

    buildInputs = { pkgs, sources, ... }:
      let
        softfloat = pkgs.callPackage ../../nix-expressions/softfloat {
          source = sources.berkeley-softfloat-3;
        };
      in
      [
        pkgs.binutils
        pkgs.clang_11
        pkgs.libiconv
        pkgs.libffi
        # pkgs.llvm_11
        pkgs.zlib
        softfloat
      ];

    overlays = { sources }:
      [
        # NOTE: abc-verifier in nixpkgs does not package its lib and include, just the exe
        # (self: super: { abc = self.abc-verifier; })
        (self: super: {
          softfloat1 = self.callPackage ../../nix-expressions/softfloat {
            source = sources.berkeley-softfloat-3;
          };
        })
      ];

    packages = pkgs:
      [
        pkgs.bv-sized
        pkgs.bv-sized-float
        pkgs.grift
        pkgs.macaw-aarch32
        pkgs.macaw-aarch32-symbolic
        pkgs.macaw-semmc
        pkgs.macaw-ppc
        pkgs.macaw-ppc-symbolic
        pkgs.macaw-refinement
        pkgs.macaw-riscv
        pkgs.macaw-symbolic
        pkgs.macaw-x86
        pkgs.macaw-x86-symbolic
        pkgs.semmc
      ];

  })
