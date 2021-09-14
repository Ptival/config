{ src }:

import ../haskell-scaffolding.nix (rec {

  name = "daedalus";
  inherit src;

  buildInputs = { pkgs, ... }:
    [
      pkgs.nodejs_latest
    ];

  lookupSha256 = { location, tag, ... }:
    {
      "https://github.com/haskell/lsp" = {
        "6af22068b22051a6e8c9964bc179f8f02ee3bf77" = "0zz1nvabnsn41xc2w9ym7hbgxf610x9wcmiv7hmwm7djqqpwmfib";
      };
    }."${location}"."${tag}";

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

  sourceFilter = { filters, ... }: filters.nodeModulesFilter;

})
