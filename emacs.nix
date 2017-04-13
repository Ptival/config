{ config, pkgs, ... }:

let emacsImageMagick = pkgs.emacs.override (with pkgs; {
  inherit imagemagick;
  withGTK2 = true;
  withGTK3 = false;
}); in

let myEmacs = (pkgs.emacsPackagesNgGen emacsImageMagick).emacsWithPackages (epkgs:
  (with epkgs.melpaPackages;
    [
      bind-key
      color-theme
      company-cabal
      company-coq
      company-ghc
      company-ghci
      company-nixos-options
      evil
      evil-leader
      git-timemachine
      git-auto-commit-mode
      git-gutter
      gitattributes-mode
      gitconfig-mode
      gitignore-mode
      hamlet-mode
      haskell-mode
      markdown-mode
      mouse3
      nyan-mode
      rainbow-delimiters
      tuareg
    ]
  ) ++
  (with pkgs.emacsPackages;
    [
      proofgeneral_HEAD
    ]
  ) ++
  (with pkgs.emacsPackagesNg;
    [
      ghc-mod
    ]
  )
);
in {
  environment.systemPackages = [
    myEmacs
  ];
}
