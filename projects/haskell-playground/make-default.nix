{ src }:
import ../haskell-scaffolding.nix (rec {
  name = "haskell-playground";
  inherit src;
})
