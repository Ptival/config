{ src }:

import ../haskell-scaffolding.nix (rec {

  name = "saw-script";
  inherit src;

  buildInputs = { pkgs, set, ... }:
    [
      pkgs.clang
      pkgs.llvm
      pkgs.yices
      pkgs.z3
      set.cryptol.components.exes.cryptol
      set.saw-script.components.exes.saw
    ];

  overlays = { sources }:
    [
      # NOTE: abc-verifier in nixpkgs does not package its lib and include, just the exe
      # (self: super: { abc = self.abc-verifier; })
      (self: super: { abc = self.callPackage ../../nix-expressions/abc { source = sources.abc; }; })
    ];

  lookupSha256 = { location, tag, ... }:
    {
      "https://github.com/eddywestbrook/hobbits.git"."e5918895396b6bcee2fc39f6bd0d77a90a52ba5f" =
        "0qh1b3z6n7afgsd1zzsy8crrx00p7k31yy3jhckmzmpfpiknkl8m";
    }."${location}"."${tag}";

  modules = info@{ macOSWorkaround, pkgs, ... }:
    let
      preConfigureWorkaround = ''
        export DYLD_INSERT_LIBRARIES=${macOSWorkaround}/macos11ghcwa.dylib
      '';
    in
    [
      {
        # packages.abcBridge.components.library.build-tools = [
        #   pkgs.abc-verifier
        # ];
        packages.cryptol-saw-core.components.library.preConfigure = preConfigureWorkaround;
        packages.saw-core-coq.components.library.preConfigure = preConfigureWorkaround;
        # Can't find the right way to disable tests entirely
        # packages.saw-remote-api.doCheck = false;
        # packages.saw-remote-api.components.tests.test-saw-remote-api.doCheck = false;
        packages.saw-script.components.library.preConfigure = preConfigureWorkaround;
      }
    ];

  packages = pkgs:
    [
      pkgs.crucible
      pkgs.crux
      pkgs.cryptol
      pkgs.parameterized-utils
      pkgs.saw-core
      pkgs.${name}
    ];

})
