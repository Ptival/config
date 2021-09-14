{ src }:

let

  my-latex = pkgs:
    let
      gensymb.pkgs = [ (pkgs.callPackage ./gensymb.nix { }) ];
    in
    pkgs.texlive.combine
      {
        inherit (pkgs.texlive)
          # schemes contain many packages
          scheme-medium
          # then we can add more packages manually
          euenc
          fontspec
          inconsolata
          newunicodechar
          preview
          standalone
          upquote
          xetex
          # xetex-def
          ;
        inherit gensymb;
      };

in

import ../haskell-scaffolding.nix
  (rec {

    compiler-nix-name = "ghc8104";
    name = "reanimate-3b1b-morphisms";
    inherit src;

    buildInputs = { pkgs, ... }:
      [
        pkgs.dejavu_fonts
        pkgs.fontconfig
        (my-latex pkgs)
      ]
      ++
      pkgs.lib.optionals pkgs.stdenv.isDarwin
        [
          pkgs.darwin.apple_sdk.frameworks.ApplicationServices
          pkgs.darwin.apple_sdk.frameworks.Cocoa
          pkgs.darwin.apple_sdk.frameworks.CoreServices
        ];

    shellHook = { pkgs, ... }:
      let
        fontConfig = pkgs.makeFontsConf {
          fontDirectories = [
            pkgs.lmodern
          ];
        };
      in
      ''
        export FONTCONFIG_FILE=${fontConfig};
      '';

    # modules = info@{ macOSWorkaround, pkgs, ... }:
    #   let
    #     preConfigureWorkaround = ''
    #       export DYLD_INSERT_LIBRARIES=${macOSWorkaround}/macos11ghcwa.dylib
    #     '';
    #   in
    #   [
    #     {
    #       packages.chiphunk.components.library.preConfigure = preConfigureWorkaround;
    #     }
    #   ];

    overlays = info: [
      (self: super: {
        m = self.openlibm;
      })
    ];

    packages = pkgs:
      [
        pkgs.${name}
      ];

  })
