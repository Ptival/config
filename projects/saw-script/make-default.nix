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
  compiler-nix-name = "ghc8102";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix { };
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = { hackage = import (fetchNiv "hackage.nix"); };
  };

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [ (self: super: {
      abc = self.callPackage ./nix/abc {};
    }) ];
  });

  # hls-set = pkgs.haskell-nix.cabalProject {
  #   src = pkgs.fetchFromGitHub {
  #     name = "haskell-language-server";
  #     inherit (sources.haskell-language-server) owner repo rev;
  #     # Need to override the hash due to lack of niv submodule support
  #     sha256 = "0vkh5ff6l5wr4450xmbki3cfhlwf041fjaalnwmj7zskd72s9p7p";
  #     fetchSubmodules = true;
  #   };
  #   # src = fetchNiv "haskell-language-server";
  #   lookupSha256 = { location, tag, ... } : {
  #     "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
  #   }."${location}"."${tag}";
  #   inherit compiler-nix-name; # index-state; # checkMaterialization;
  #   # Plan issues with the benchmarks, can try removing later
  #   configureArgs = "--disable-benchmarks";
  #   # Invalidate and update if you change the version
  #   plan-sha256 = "0ga7a2d22hbxvhx4363g0iyss4x5kaamkxh7bhmcki6azsha92vz";
  #   modules = [{
  #     # Tests don't pass for some reason, but this is a somewhat random revision.
  #     packages.haskell-language-server.doCheck = false;
  #   }];
  # };

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.cleanSourceHaskell {
      inherit name src;
      # subDir = "";
    };

  };

in set.${name}.components.library // {

  shell = set.shellFor {

    packages = ps:
      with ps;
      [
        ps.crucible
        ps.cryptol
        ps.${name}
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
    exactDeps = true;

  };

}
