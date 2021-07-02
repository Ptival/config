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

  name = "macaw";
  compiler-nix-name = "ghc8104";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix { };
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = { hackage = import (fetchNiv "hackage.nix"); };
  };

  pkgs = import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays
      ++ [
      (self: super: {
        softfloat1 = self.callPackage ../../nix-expressions/softfloat {
          source = sources.berkeley-softfloat-3;
        };
      })
    ];
  });

  hls-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      name = "haskell-language-server";
      inherit (sources.haskell-language-server) owner repo rev;
      # Need to override the hash due to lack of niv submodule support
      sha256 = "0gpbk0si0gvk5bahdig90mwcvzyq7kbxnszxnyjc5xnvb3y5pnmw";
      fetchSubmodules = true;
    };
    # src = fetchNiv "haskell-language-server";
    lookupSha256 = { location, tag, ... }:
      {
        "https://github.com/hsyl20/ghc-api-compat"."8fee87eac97a538dbe81ff1ab18cff10f2f9fa15" =
          "16bibb7f3s2sxdvdy2mq6w1nj1lc8zhms54lwmj17ijhvjys29vg";
      }."${location}"."${tag}";
    inherit compiler-nix-name; # index-state; # checkMaterialization;
    # Plan issues with the benchmarks, can try removing later
    configureArgs = "--disable-benchmarks";
    # Invalidate and update if you change the version
    plan-sha256 = "00bbr66bzjzb81g15l70xmd110axllxakh6dp1b6p5s334qa95ww";
    modules = [{
      # Tests don't pass for some reason, but this is a somewhat random revision.
      packages.haskell-language-server.doCheck = false;
    }];
  };

  softfloat = pkgs.callPackage ../../nix-expressions/softfloat {
    source = sources.berkeley-softfloat-3;
  };

  preConfigureWorkaround =
    let
      workaround = pkgs.callPackage ./macos11-haskell-workaround {
        source = sources.macos11-haskell-workaround;
      };
    in
    ''
      export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:${softfloat}/lib
      export DYLD_INSERT_LIBRARIES=${workaround}/macos11ghcwa.dylib
    '';

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.cleanSourceHaskell {
      inherit name src;
      # subDir = "";
    };

    pkg-def-extras = [
      (hackage: {
        packages = {
          # containers = hackage.containers."0.5.11.0".revisions.default;
        };
      })
    ];

    modules =
      [
        # {
        #   packages.bv-sized-float.components.library.preConfigure = preConfigureWorkaround;
        #   packages.grift.components.library.preConfigure = preConfigureWorkaround;
        #   packages.macaw-base.components.library.preConfigure = preConfigureWorkaround;
        #   packages.macaw-riscv.components.library.preConfigure = preConfigureWorkaround;
        #   packages.softfloat-hs.components.library = {
        #     # includeDirs = [
        #     #   "${softfloat}/include"
        #     # ];
        #     # preConfigure = preConfigureWorkaround;
        #   };
        # }
      ];

  };

in
set // {

  shell = set.shellFor {

    buildInputs = [
      hls-set.ghcide.components.exes.ghcide
      hls-set.haskell-language-server.components.exes.haskell-language-server
      pkgs.binutils
      pkgs.clang_11
      # pkgs.llvm_11
      pkgs.zlib
      softfloat
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # exactDeps = true;

    name = "macaw";

    packages = ps:
      with ps; [
        ps.bv-sized
        ps.bv-sized-float
        ps.grift
        # ps.macaw-aarch32
        # ps.macaw-aarch32-symbolic
        # ps.macaw-semmc
        ps.macaw-ppc
        # ps.macaw-ppc-symbolic
        # ps.macaw-refinement
        ps.macaw-riscv
        # ps.macaw-symbolic
        # ps.macaw-x86
        # ps.macaw-x86-symbolic
        # ps.semmc
      ];

    # Need this so that cabal in HLS has access to the dynamic libraries
    # shellHook = ''
    #   export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:${softfloat}/lib
    #   export DYLD_FALLBACK_LIBRARY_PATH=${softfloat}/lib
    # '';

    tools = {
      cabal = "3.2.0.0";
      cabal-fmt = "0.1.5";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

    withHoogle = true;

  };

}
