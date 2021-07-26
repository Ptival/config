{ src }:

import ../haskell-scaffolding.nix (rec {

  compiler-nix-name = "ghc8104";
  name = "daedalus";
  inherit src;

  buildInputs = { pkgs, ... }:
    [
      pkgs.nodejs_latest
    ];

  modules = { ... }:
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
        '';

      }
    ];

  packages = pkgs:
    [
      pkgs.daedalus-language-server
    ];

})
