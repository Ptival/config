{ src }:
import ../haskell-scaffolding.nix (rec {
  name = "nodegroups";
  inherit src;
})
