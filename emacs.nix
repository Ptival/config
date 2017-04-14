{ config, pkgs, ... }:

let emacsImageMagick = pkgs.emacs.override (with pkgs; {
  inherit imagemagick;
  withGTK2 = true;
  withGTK3 = false;
}); in

let myEmacs = (pkgs.emacsPackagesNgGen emacsImageMagick).emacsWithPackages (epkgs:
  (with epkgs.melpaPackages;
    [
      bind-key              # useful to bind some Haskell shortcuts
      color-theme
      company-cabal
      company-coq
      company-ghc
      company-ghci
      company-nixos-options
      evil
      evil-leader
      #git-timemachine
      git-gutter            # show git information in emacs' gutter
      gitconfig-mode        # helps when editing .git/config
      gitignore-mode        # helps when editing .gitignore
      hamlet-mode
      haskell-mode
      intero
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
    ]
  )
);
in {
  environment.systemPackages = [
    myEmacs
  ];
}
