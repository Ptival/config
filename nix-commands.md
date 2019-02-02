Lookup Haskell-related packages:
  nix-env -f "<nixpkgs>" -qaP -A haskellPackages

Search for a package:
  nix-env -qaP | grep package

Install package from local repository:
  nix-env -i package -f /path/to/nixpkgs

Remove a package:
  nix-env -e package

Try to build a Haskell package:
  nix-build -A haskell.packages.ghc810.Glob '<nixpkgs>'

Try to build some package:
  nix-build '<nixpkgs>' -A package

Set yourself in an environment like the one to build a package:
  nix-shell --pure -p package --command sh

To do some bisecting on nixpkgs:
First, comment out non-important packages in configuration.nix, then:
  sudo nixos-rebuild switch -I nixpkgs=/path/to/nixpkgs
Reboot if dealing with something startup-related

Find a package specific to a language module:
  nix-env -f "<nixpkgs>" -qaP -A haskellPackages
  nix-env -f "<nixpkgs>" -qaP -A coqPackages_8_6

To try and delete a store path (whether derivation or output):
  nix-store --delete /nix/store/path/to/thing/thing{,.drv}

To figure out why something can't be deleted (or what depends on it):
  nix-store --query --roots /nix/store/path/to/thing/thing{,.drv}
