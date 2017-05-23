{ config, pkgs, ... }:

let emacsImageMagick = pkgs.emacs.override (with pkgs; {
  inherit imagemagick;
  withGTK2 = true;
  withGTK3 = false;
}); in

let myEmacs = (pkgs.emacsPackagesNgGen emacsImageMagick).emacsWithPackages (epkgs:
  (with epkgs.melpaPackages;
    [
      #   bind-key                # useful to bind some Haskell shortcuts
      #   color-theme
      #   company-cabal
      #   company-coq
      #   company-ghc
      #   company-ghci
      #   company-nixos-options
      #   elm-mode
      #   #evil                   # transiently broken
      #   #evil-leader            # transiently broken
      #   flycheck-elm
      #   flycheck-purescript
      #   #git-timemachine
      #   git-gutter              # show git information in emacs' gutter
      #   gitconfig-mode          # helps when editing .git/config
      #   gitignore-mode          # helps when editing .gitignore
      #   #hamlet-mode            # transiently broken, useless to me at the moment
      #   haskell-mode
      #   idris-mode
      #   intero
      #   markdown-mode
      #   #mouse3                 # transiently broken
      #   nyan-mode
      #   psc-ide
      #   purescript-mode
      #   rainbow-delimiters
      #   #tide
      #   tuareg
    ]
  ) ++
  (with pkgs.emacsPackages;
    [
      proofgeneral_HEAD
    ]
  ) ++
  (with pkgs.emacsPackagesNg;
    [
      #structured-haskell-mode
    ]
  )
);
in {
  environment.systemPackages = [
    myEmacs
  ];
}
