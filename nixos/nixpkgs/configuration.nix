rec {

  niv = source: fetchTarball { inherit (source) url sha256; };

  nur = pkgs.callPackage (niv sources.NUR) {};

  pkgs = import (niv sources.nixpkgs) {
      config = {
        allowUnfree = true;
      };
      overlays = [
        (import (niv sources.emacs-overlay))
      ];
    };

  sources = import ./nix/sources.nix {};

}
