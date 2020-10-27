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
      fzf
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
      enableAutosuggestions = true;
      enableCompletion = true;
      # enableVteIntegration = true;

      initExtra = ''
# This needs to be sourced using nix, but not on NixOS
if [[ -f /home/val/.nix-profile/etc/profile.d/nix.sh ]]; then
  . /home/val/.nix-profile/etc/profile.d/nix.sh
fi
cd ~
eval "$(direnv hook zsh)"
${builtins.readFile ../dotfiles/p10k.zsh}
source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
export FZF_BASE=${pkgs.fzf}
      '';

      oh-my-zsh = {
        enable = true;
        plugins = [
          "cabal"
          "fzf"
          "git"
          "sudo"
          # "zsh-syntax-highlighting"
        ];
      };

      plugins =
        [
          {
            name = "enhancd";
            file = "init.sh";
            src = pkgs.fetchFromGitHub {
              owner = "b4b4r07";
              repo = "enhancd";
              rev = "v2.2.1";
              sha256 = "0iqa9j09fwm6nj5rpip87x3hnvbbz9w9ajgm6wkrd5fls8fn8i5g";
            };
          }
          {
            name = "zsh-autosuggestions";
            src = pkgs.fetchFromGitHub {
              owner = "zsh-users";
              repo = "zsh-autosuggestions";
              rev = "v0.4.0";
              sha256 = "0z6i9wjjklb4lvr7zjhbphibsyx51psv50gm07mbb0kj9058j6kc";
            };
          }
        ];

    };

  };

}
