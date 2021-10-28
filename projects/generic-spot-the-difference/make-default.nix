{ src }:
import ../haskell-scaffolding.nix (rec {
  name = "generic-spot-the-differences";
  inherit src;
})
