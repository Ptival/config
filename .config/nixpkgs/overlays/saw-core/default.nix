self: super:
{
  saw-core = super.callPackage ~/Galois/saw/saw-core.nix { nixpkgs = self; };
}
