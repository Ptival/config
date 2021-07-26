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

  name = "haskell-playground";
  compiler-nix-name = "ghc8104";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix { };
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = { hackage = import (fetchNiv "hackage.nix"); };
  };

  pkgs = import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
  # pkgs = import sources.nixpkgs (haskellNix.nixpkgsArgs // {
  # pkgs = import haskellNix.sources.nixpkgs-2009 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays
      ++ [
        # NOTE: abc-verifier in nixpkgs does not package its lib and include, just the exe
        # (self: super: { abc = self.abc-verifier; })
        (self: super: { abc = self.callPackage ../../nix-expressions/abc { source = sources.abc; }; })
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

  workaround = pkgs.callPackage ./macos11-haskell-workaround {
    source = sources.macos11-haskell-workaround;
  };

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.cleanSourceHaskell {
      inherit name src;
      # subDir = "";
    };

    modules =
      let
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
          # Can't find the right way to disable tests entirely
          # packages.saw-remote-api.doCheck = false;
          # packages.saw-remote-api.components.tests.test-saw-remote-api.doCheck = false;
          packages.saw-script.components.library.preConfigure = preConfigureWorkaround;
        }
      ];

  };

in set // {

  shell = set.shellFor {

    buildInputs = [
      hls-set.haskell-language-server.components.exes.haskell-language-server
    ];

    DYLD_INSERT_LIBRARIES="${workaround}/macos11ghcwa.dylib";

    inherit name;

    packages = ps:
      with ps; [
        ps.${name}
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
