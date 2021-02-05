# You can sym-link to this file in the actual project directory.
# You can then create a `default.nix` with contents:
#
#   import ./make-default.nix { src = ./.; }
#
# You can also create a `shell.nix` with contents:
#
#   (import ./.).shell
{ src }:
let

  name = "saw-script";
  compiler-nix-name = "ghc8103";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix { };
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = { hackage = import (fetchNiv "hackage.nix"); };
  };

  pkgs = import sources.nixpkgs (haskellNix.nixpkgsArgs // {
  # pkgs = import haskellNix.sources.nixpkgs-2009 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays
      ++ [
        # NOTE: abc-verifier in nixpkgs does not package its lib and include, just the exe
        # (self: super: { abc = self.abc-verifier; })
        (self: super: { abc = self.callPackage ./nix/abc { }; })
      ];
  });

  hls-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      name = "haskell-language-server";
      inherit (sources.haskell-language-server) owner repo rev;
      # Need to override the hash due to lack of niv submodule support
      sha256 = "18g0d7zac9xwywmp57dcrjnvms70f2mawviswskix78cv0iv4sk5";
      fetchSubmodules = true;
    };
    # src = fetchNiv "haskell-language-server";
    lookupSha256 = { location, tag, ... }:
      {
        "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" =
          "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
        "https://github.com/alanz/ghc-exactprint.git"."6748e24da18a6cea985d20cc3e1e7920cb743795" =
          "18r41290xnlizgdwkvz16s7v8k2znc7h215sb1snw6ga8lbv60rb";
      }."${location}"."${tag}";
    inherit compiler-nix-name; # index-state; # checkMaterialization;
    # Plan issues with the benchmarks, can try removing later
    configureArgs = "--disable-benchmarks";
    # Invalidate and update if you change the version
    plan-sha256 = "1amd018md8i7lm253wi311vszg14f2icafw85id456jdimzl5sp9";
    modules = [{
      # Tests don't pass for some reason, but this is a somewhat random revision.
      packages.haskell-language-server.doCheck = false;
    }];
  };

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.cleanSourceHaskell {
      inherit name src;
      # subDir = "";
    };

    modules =
      let
        workaround = pkgs.callPackage ./nix/macos11-haskell-workaround {};
        preConfigureWorkaround = ''
          export DYLD_INSERT_LIBRARIES=${workaround}/macos11ghcwa.dylib
        '';
      in
      [
        {
          # packages.abcBridge.components.library.build-tools = [
          #   pkgs.abc-verifier
          # ];
          packages.cryptol-saw-core.components.library.preConfigure = preConfigureWorkaround;
          packages.saw-core-coq.components.library.preConfigure = preConfigureWorkaround;
          packages.saw-script.components.library.preConfigure = preConfigureWorkaround;
        }
      ];

  };

in set // {

  shell = set.shellFor {

    buildInputs = [
      hls-set.haskell-language-server.components.exes.haskell-language-server
      pkgs.clang
      pkgs.llvm
      pkgs.yices
      pkgs.z3
      set.cryptol.components.exes.cryptol
      set.saw-script.components.exes.saw
    ];

    DYLD_INSERT_LIBRARIES="${pkgs.callPackage ./nix/macos11-haskell-workaround {}}/macos11ghcwa.dylib";

    packages = ps:
      with ps; [
        ps.abcBridge
        ps.crucible
        ps.crux
        ps.cryptol
        # ps.flexdis86
        # ps.jvm-verifier
        # ps.llvm-pretty
        ps.parameterized-utils
        ps.saw-core
        ps.${name}
        # ps.what4
      ];

    withHoogle = true;

    tools = {
      cabal = "3.2.0.0";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # exactDeps = true;

  };

}
