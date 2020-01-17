let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{
  packageOverrides = pkgs: {
    # To install below "pseudo-package", run:
    #  $ nix-env -i all
    all-hies = pkgs.buildEnv {
      name = "haskell-ide-engine";
      paths = [
        (all-hies.selection {
        # Alternatively:
        # (all-hies.bios.selection {
          selector = p: {
            inherit (p) ghc844;
            inherit (p) ghc865;
          };
        })
      ];
    };
  };
}
