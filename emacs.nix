{ config, pkgs, ... }:

let emacsImageMagick = pkgs.emacs.override (with pkgs; {
  inherit imagemagick;
}); in

let myEmacs = (pkgs.emacsPackagesNgGen emacsImageMagick).emacsWithPackages (
  with pkgs.emacsPackages;   # has proofgeneral_HEAD and such...
  with pkgs.emacsPackagesNg; # has color-theme and such...
  [

    bind-key

    color-theme

    company
    #company-nixos-options
    #company-cabal
    company-coq
    company-ghc
    company-ghci

    #evil
    #evil-leader
    ghc-mod

    #git-timemachine
    #git-auto-commit-mode
    #git-gutter
    #gitattributes-mode
    #gitconfig-mode
    #gitignore-mode
    #hamlet-mode
    haskell-mode
    #helm
    #magit
    #markdown-mode
    #mouse3
    nyan-mode

    #ocp-indent

    #org-plus-contrib
    #org-gnome
    #org-password-manager

    proofgeneral_HEAD
    rainbow-delimiters
    #tuareg
    #undo-tree

  ]
);
in {
  environment.systemPackages = [
    myEmacs
  ];
}
