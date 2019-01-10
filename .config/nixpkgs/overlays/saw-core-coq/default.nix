self: super:
{
  saw-core-coq = super.callPackage ~/Galois/saw/saw-core-coq.nix { pkgs = self; };
}
