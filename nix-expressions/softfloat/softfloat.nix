let
  sources = import ../nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};
in
nixpkgs.callPackage ./default.nix {}
