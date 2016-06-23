{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithPackages (self : [
      self.ghc-mod
    ]))
  ];

}
