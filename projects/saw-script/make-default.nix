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
  compiler-nix-name = "ghc884";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix { };
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = { hackage = import (fetchNiv "hackage.nix"); };
  };

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays
      ++ [ (self: super: { abc = self.callPackage ./nix/abc { }; }) ];
  });

  hls-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      name = "haskell-language-server";
      inherit (sources.haskell-language-server) owner repo rev;
      # Need to override the hash due to lack of niv submodule support
      sha256 = "0p6fqs07lajbi2g1wf4w3j5lvwknnk58n12vlg48cs4iz25gp588";
      fetchSubmodules = true;
    };
    # src = fetchNiv "haskell-language-server";
    lookupSha256 = { location, tag, ... }:
      {
        "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" =
          "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
      }."${location}"."${tag}";
    inherit compiler-nix-name; # index-state; # checkMaterialization;
    # Plan issues with the benchmarks, can try removing later
    configureArgs = "--disable-benchmarks";
    # Invalidate and update if you change the version
    plan-sha256 = "03h35lhz1ngmz0lifp9g7lchp4dy7qlqj87rc9dsivdi4g1hlcqq";
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

  };

in set // {

  shell = set.shellFor {

    buildInputs = [
      hls-set.haskell-language-server.components.exes.haskell-language-server
      pkgs.abc
      pkgs.clang
      pkgs.llvm
      pkgs.yices
      pkgs.z3
      set.cryptol.components.exes.cryptol
      set.saw-script.components.exes.saw
    ];

    packages = ps:
      with ps; [
        # ps.abcBridge
        # ps.crucible
        # ps.crux
        # ps.cryptol
        # ps.flexdis86
        # ps.jvm-verifier
        # ps.llvm-pretty
        # ps.parameterized-utils
        # ps.saw-core
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

    # buildInputs = with pkgs.haskellPackages; [ ghcid pkgs.binutils ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # exactDeps = true;

  };

}
