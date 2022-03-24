{ buildInputs ? ({ ... }: [ ])
, compiler-nix-name ? "ghc8107"
, sha256map ? {}
, modules ? ({ ... }: [ ])
, name
, overlays ? ({ sources }: [ ])
, packages ? (pkgs: [ pkgs.${name} ])
, propagatedBuildInputs ? ({ ... }: [ ])
, shellHook ? ({ ... }: "")
, sourceFilter ? ({ ... }: name: type: true)
, src
, subDir ? ""
, ...
}:

let

  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./niv-shared-sources/nix/sources.nix { };
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = { hackage = import (fetchNiv "hackage.nix"); };
  };

  # pkgs = import haskellNix.sources.nixpkgs-2105 (haskellNix.nixpkgsArgs // {
  pkgs = import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ overlays { inherit sources; };
  });

  macOSWorkaround =
    pkgs.callPackage ./macos11-haskell-workaround.nix {
      source = sources.macos11-haskell-workaround;
    };

  # A bunch of useful source filters packages may want to use
  filters =
    {
      nodeModulesFilter = name: type:
        let baseName = baseNameOf (toString name);
        in
        pkgs.haskell-nix.haskellSourceFilter name type && !(
          # this trips haskell.nix as it contains files named package.yaml
          baseName == "node_modules"
          # || other conditions...
        );
    };

  # Put here any info you want to pass to callbacks
  info = {
    inherit filters;
    inherit macOSWorkaround;
    inherit pkgs;
    inherit set;
    inherit sources;
  };

  hls-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      name = "haskell-language-server";
      inherit (sources.haskell-language-server) owner repo rev;
      # Need to override the hash due to lack of niv submodule support
      sha256 = "sha256-qcaKQRGy9fPhjHsZxsm7NGCoKUDuaJgep8fp/Z1SgL8=";
      fetchSubmodules = true;
    };
    # src = fetchNiv "haskell-language-server";
    sha256map = {
        "https://github.com/haskell/lsp.git"."ef59c28b41ed4c5775f0ab0c1e985839359cec96" =
          "1whcgw4hhn2aplrpy9w8q6rafwy7znnp0rczgr6py15fqyw2fwb5";
        "https://github.com/hsyl20/ghc-api-compat"."8fee87eac97a538dbe81ff1ab18cff10f2f9fa15" =
          "16bibb7f3s2sxdvdy2mq6w1nj1lc8zhms54lwmj17ijhvjys29vg";
      };
    inherit compiler-nix-name; # index-state; # checkMaterialization;
    # Plan issues with the benchmarks, can try removing later
    configureArgs = "--disable-benchmarks";
    # Invalidate and update if you change the version
    plan-sha256 = "sha256-8b8rWVfr2GjQQ6erq0LkiUdSQYkt43neB0fp4WS+CN0=";
    modules = [{
      # Tests don't pass for some reason, but this is a somewhat random revision.
      packages.haskell-language-server.doCheck = false;
    }];
  };

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;
    inherit sha256map;
    modules = modules info;

    src =
      pkgs.haskell-nix.haskellLib.cleanSourceWith {
        filter = sourceFilter info;
        inherit name;
        src = pkgs.lib.cleanSource src;
        inherit subDir;
      };

  };

in

set // {

  shell = set.shellFor {

    buildInputs = buildInputs info ++ [
      hls-set.ghcide.components.exes.ghcide
      hls-set.haskell-language-server.components.exes.haskell-language-server
    ];

    DYLD_INSERT_LIBRARIES="${macOSWorkaround}/macos11ghcwa.dylib";

    inherit name;

    propagatedBuildInputs = propagatedBuildInputs info;

    inherit packages;

    shellHook = shellHook info;

    tools = {
      cabal = "latest";
      cabal-fmt = "latest";
      hlint = "latest";
      # hpack = "0.34.2";
      ormolu = "0.3.1.0";
    };

    withHoogle = true;

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # exactDeps = true;

  };

}
