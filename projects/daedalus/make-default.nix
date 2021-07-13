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

  name = "daedalus";
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

    src =
      let
        mySourceFilter = name: type:
          let baseName = baseNameOf (toString name);
          in pkgs.haskell-nix.haskellSourceFilter name type && !(
            # this trips haskell.nix as it contains files named package.yaml
            baseName == "node_modules"
            # || other conditions...
          );
      in
      pkgs.lib.cleanSourceWith {
        filter = mySourceFilter;
        inherit name;
        src = pkgs.lib.cleanSource src;
      };

    pkg-def-extras = [(hackage: {
      packages = {
        # containers = hackage.containers."0.5.11.0".revisions.default;
      };
    })];

    modules =
      let
        preConfigureWorkaround = ''
          export DYLD_INSERT_LIBRARIES=${workaround}/macos11ghcwa.dylib
        '';
      in
      [
        {

          packages.pdf-cos.components.library.preBuild = ''
            cp --remove-destination ${src}/jpeg/JpegBasics.ddl ./spec/JpegBasics.ddl
          '';

          packages.pdf-driver.components.library.preBuild = ''
            cp --remove-destination ${src}/pdf-cos/spec/JpegBasics.ddl ./spec/JpegBasics.ddl
            cp --remove-destination ${src}/pdf-cos/spec/PdfDecl.ddl    ./spec/PdfDecl.ddl
            cp --remove-destination ${src}/pdf-cos/spec/PdfValue.ddl   ./spec/PdfValue.ddl
            cp --remove-destination ${src}/pdf-cos/spec/PdfXRef.ddl    ./spec/PdfXref.ddl
            cp --remove-destination ${src}/pdf-cos/spec/Stdlib.ddl     ./spec/Stdlib.ddl
            ls -lah spec
          '';

        }
      ];

  };

in set // {

  shell = set.shellFor {

    inherit name;

    buildInputs = [
      hls-set.ghcide.components.exes.ghcide
      hls-set.haskell-language-server.components.exes.haskell-language-server
      pkgs.nodejs_latest
    ];

    DYLD_INSERT_LIBRARIES="${workaround}/macos11ghcwa.dylib";

    packages = ps:
      with ps; [
        ps.daedalus-language-server
      ];

    withHoogle = true;

    tools = {
      cabal = "3.2.0.0";
      cabal-fmt = "0.1.5.1";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # exactDeps = true;

  };

}
