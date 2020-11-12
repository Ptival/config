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

  name = "crucible";
  compiler-nix-name = "ghc883";
  niv = source: fetchTarball { inherit (sources.${source}) url sha256; };

  sources = import ./nix/sources.nix {};
  haskellNix = import (niv "haskell.nix") {
    sourcesOverride = {
      hackage = niv "hackage.nix";
    };
  };
  nur = import (niv "nur") {};

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      (self: super: {
        abc = self.callPackage ./abc {};
        golang = throw "golang";
      })
    ];
  });

  hls-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      name = "haskell-language-server";
      inherit (sources.haskell-language-server) owner repo rev;
      # Need to override the hash due to lack of niv submodule support
      # 0.5.0
      # sha256 = "0vkh5ff6l5wr4450xmbki3cfhlwf041fjaalnwmj7zskd72s9p7p";
      # master
      sha256 = "1zibq81vp5h6d0y1r04fgh8l21hvbp828y0l8b9a007brqzpwhf9";
      fetchSubmodules = true;
    };
    lookupSha256 = { location, tag, ... } : {
      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
      "https://github.com/bubba/hie-bios.git"."cec139a1c3da1632d9a59271acc70156413017e7" = "1iqk55jga4naghmh8zak9q7ssxawk820vw8932dhympb767dfkha";
    }."${location}"."${tag}";
    inherit compiler-nix-name; # index-state; # checkMaterialization;
    # Plan issues with the benchmarks, can try removing later
    configureArgs = "--disable-benchmarks";
    # Invalidate and update if you change the version
    # 0.5.0
    # plan-sha256 = "0ga7a2d22hbxvhx4363g0iyss4x5kaamkxh7bhmcki6azsha92vz";
    # master
    plan-sha256 = "0828xria5skvgnnxnnxlw78l95w2a5cdsjp0hc49rwxfc853v3lc";
    modules = [{
      # Tests don't pass for some reason, but this is a somewhat random revision.
      packages.haskell-language-server.doCheck = false;
    }];
  };

  implicit-hie-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      name = "implicit-hie";
      inherit (sources.implicit-hie) owner repo rev sha256;
    };
    # lookupSha256 = { location, tag, ... } : {
    #   "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
    #   "https://github.com/bubba/hie-bios.git"."cec139a1c3da1632d9a59271acc70156413017e7" = "1iqk55jga4naghmh8zak9q7ssxawk820vw8932dhympb767dfkha";
    # }."${location}"."${tag}";
    inherit compiler-nix-name; # index-state; # checkMaterialization;
    plan-sha256 = "0828xria5skvgnnxnnxlw78l95w2a5cdsjp0hc49rwxfc853v342";
  };

  crucible-set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name src;
      subDir = "";
    };

    pkg-def-extras = [
      (hackage:
        let default = name: version: hackage.${name}.${version}.revisions.default;
        in
        {
          packages = {
            fourmolu = default "fourmolu" "0.1.0.0";
            happy = default "happy" "1.19.12";
            implicit-hie = default "implicit-hie" "0.1.2.0";
            shake = default "shake" "0.18.4";
          };
        })
    ];

  };

in

{

  shell = crucible-set.shellFor {

    buildInputs = [
      crucible-set.crux-llvm.components.exes.crux-llvm
      hls-set.haskell-language-server.components.exes.haskell-language-server
      implicit-hie-set.implicit-hie.components.exes.gen-hie
      pkgs.abc
      pkgs.clang
      pkgs.llvm
      pkgs.yices
      pkgs.z3
    ];

    packages = p: [
      # p.abcBridge
      # p.aig
      # p.blt
      p.crucible
      p.crucible-go
      p.crucible-jvm
      p.crucible-llvm
      p.crucible-mc
      p.crucible-saw
      p.crucible-server
      p.crucible-syntax
      p.crux
      p.crux-llvm
      p.crux-mir
      p.cryptol
      # p.golang
      # p.hpb
      p.jvm-parser
      p.llvm-pretty
      p.llvm-pretty-bc-parser
      # p.rme
      p.saw-core
      p.saw-core-aig
      p.saw-core-sbv
      p.saw-core-what4
      p.what4
      p.what4-abc
      # p.what4-blt
    ];

    tools = {
      cabal = {
        inherit compiler-nix-name;
        version = "3.2.0.0";
      };
      # happy = "1.19.12";
      hie-bios = "0.7.0";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
      # stack = "2.1.3.1";
    };

  };

}
