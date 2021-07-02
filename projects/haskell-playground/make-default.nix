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
      sha256 = "0kviq3kinm3i0qm4r26rdnlkwbs1s3r1rqiqdry517rgkgnjpcp5";
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
    plan-sha256 = "0hjk22sxa0i3ix8mprkzl00q1v938hkzh63ml0dc5a4kybh8jqxq";
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
