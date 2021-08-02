{ buildInputs ? ({ ... }: [ ])
, compiler-nix-name
, lookupSha256 ? ({ ... }: null)
, modules ? ({ ... }: [ ])
, name
, overlays ? ({ sources }: [ ])
, packages ? (pkgs: [ ])
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

  pkgs = import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ overlays { inherit sources; };
  });

  # Put here any info you want to pass to callbacks
  info = {
    inherit pkgs;
    inherit set;
    inherit sources;
  };

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

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;
    inherit lookupSha256;
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

    inherit name;
    inherit packages;

    tools = {
      cabal = "3.2.0.0";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

    withHoogle = true;

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # exactDeps = true;

  };

}
