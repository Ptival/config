let

  name = "crucible";
  compiler-nix-name = "ghc883";

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchTarball { inherit (sources."haskell.nix") url sha256; }) {
    sourcesOverride = {
      hackageSrc = fetchTarball { inherit (sources."hackage.nix") url sha256; };
    };
  };
  all-hies = import (fetchTarball { inherit (sources.all-hies) url sha256; }) {};
  nur = import (fetchTarball { inherit (sources.nur) url sha256; }) {};

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      all-hies.overlay
      (self: super: {
        abc = self.callPackage ./nix/abc {};
      })
    ];
  });

  project = pkgs.haskell-nix.cabalProject {
      inherit compiler-nix-name;
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        inherit name;
        src = ./.;
        subDir = "";
      };
    };

in

{

  crux-llvm = project.crux-llvm.components.exes.crux-llvm;

  shell = project.shellFor {

    buildInputs = [
      # add development packages here
      pkgs.clang
      # pkgs.haskell.packages.${compiler-nix-name}.haskell-language-server
      pkgs.llvm
      pkgs.yices
      pkgs.z3
    ];

    packages = p: [
      # p.crucible
      # p.crucible-jvm
      # p.crucible-llvm
      # p.crucible-mc
      # p.crucible-saw
      # p.crucible-server
      # p.crucible-syntax
      # p.crux
      p.crux-llvm
    ];

    tools = {
      # c2hs = "0.28.6";
      cabal = "3.2.0.0";
      hie = "unstable";
      hlint = "2.2.11";
      # hpack = "0.34.2";
      # ormolu = "1.2.3";
    };

  };

}
