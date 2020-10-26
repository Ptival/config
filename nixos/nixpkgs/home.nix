# { config, lib, pkgs, ... }:

let

  sources = import ./nix/sources.nix {};
  fetchNiv = niv: fetchTarball { inherit (niv) url sha256; };
  nur = pkgs.callPackage (fetchNiv sources.NUR) {};

  pkgs = import (fetchNiv sources.nixpkgs) {
      config = {
        allowUnfree = true;
      };
      overlays = [
        (import (fetchNiv sources.emacs-overlay))
      ];
    };

  doom-emacs = pkgs.callPackage (fetchNiv sources.nix-doom-emacs) {
    doomPrivateDir = ../dotfiles/doom.d;
    emacsPackages = (pkgs.emacsPackagesNgGen
      (pkgs.emacsGit.override {
        inherit (pkgs) imagemagick;
        withGTK3 = true;
        withXwidgets = true;
      }));
    extraPackages = epkgs: [ pkgs.emacsPackages.proofgeneral_HEAD ];
  };

  iosevkass09 = pkgs.iosevka.override {
    privateBuildPlan = {
      design = [ "ss09" ];
      family = "Iosevka SS09";
    };
    set = "ss09";
  };

  mesloNerdP10k = nur.repos.ptival.meslo-nerd-powerlevel10k;

in

{

  # Does not work on NixOS with useGlobalPkgs
  # fonts.fontconfig.enable = true;

  home = {
    packages = with pkgs; [
      bat               # Nicer cat
      binutils
      cabal2nix
      cachix
      dejavu_fonts
      doom-emacs
      emacs-all-the-icons-fonts
      fd                # Makes file search faster in doom-emacs
      fontconfig
      fzf-zsh           # Fuzzy line-finder for zsh
      git
      gitg
      gitAndTools.delta # Nicer pager
      gnumake
      home-manager
      htop              # Nicer top
      iosevkass09
      jq                # JSON viewer
      less              # Better than busybox's less
      lorri
      mesloNerdP10k
      noto-fonts-emoji
      nixfmt            # Formatter for nix code
      niv
      openssl
      # (import ./texlive.nix {})
      ripgrep           # Better grep
      wget
      yq                # YAML viewer
      zsh-powerlevel10k
    ];
  };

  # NOTE: this does not exist when `useGlobalPkgs` is set
      # nixpkgs.config = {
      #   allowUnfree = true;
      # };

  programs = {

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    zsh = {
      enable = true;
      initExtra = ''
# This needs to be sourced using nix, but not on NixOS
if [[ -f /home/val/.nix-profile/etc/profile.d/nix.sh ]]; then
  . /home/val/.nix-profile/etc/profile.d/nix.sh
fi
cd ~
eval "$(direnv hook zsh)"
${builtins.readFile ../dotfiles/p10k.zsh}
source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      '';
      oh-my-zsh = {
        enable = true;
        plugins = [
          "git"
        ];
      };
    };

  };

}
