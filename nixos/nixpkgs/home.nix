# { config, lib, pkgs, ... }:

let

  sources = import ./nix/sources.nix {};
  niv = source: fetchTarball { inherit (source) url sha256; };
  nur = pkgs.callPackage (niv sources.NUR) {};

  pkgs = import (niv sources.nixpkgs) {
      config = {
        allowUnfree = true;
      };
      overlays = [
        (import (niv sources.emacs-overlay))
      ];
    };

  doom-emacs = pkgs.callPackage (niv sources.nix-doom-emacs) {
    doomPrivateDir = ../dotfiles/doom.d;
    emacsPackages = (pkgs.emacsPackagesNgGen
      (pkgs.emacsGit.override {
        inherit (pkgs) imagemagick;
        withGTK3 = true;
        withXwidgets = true;
      }));
    extraPackages = epkgs: [ pkgs.emacsPackages.proofgeneral_HEAD ];
  };

  # myHomeManager = import (niv sources.home-manager) {};

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
      fasd
      fd                # Makes file search faster in doom-emacs
      fontconfig
      fzf
      # fzf-zsh           # Fuzzy line-finder for zsh
      git
      gitg
      gitAndTools.delta # Nicer pager
      gnumake
      htop              # Nicer top
      iosevkass09
      jq                # JSON viewer
      less              # Better than busybox's less
      lorri
      mesloNerdP10k
      # myHomeManager
      noto-fonts-emoji
      nixfmt            # Formatter for nix code
      pkgs.niv
      openssl
      # (import ./texlive.nix {})
      ripgrep           # Better grep
      wget
      yq                # YAML viewer
      # zsh-powerlevel10k
    ];
  };

  # NOTE: this does not exist when `useGlobalPkgs` is set
      # nixpkgs.config = {
      #   allowUnfree = true;
      # };

  programs = {

    direnv = {
      enable = true;
      enableFishIntegration = true;
      # enableZshIntegration = true;
    };

    fish = {
      enable = true;

      interactiveShellInit = ''
      # Windows terminal wants to start us in C:/Users/<User>/...
      cd ~
      '';

      plugins = [

        # {
        #   name = "coffeeandcode";
        #   src = niv sources.theme-coffeeandcode;
        # }

        # /!\ WARNING: themes requires setting xdg down below!
        {
          name = "bobthefish";
          src = niv sources.theme-bobthefish;
        }

        # receive notification when long process is done
        {
          name = "done";
          src = niv sources.done;
        }

        {
          name = "fasd";
          src = niv sources.plugin-fasd;
        }

        # needs fish >= 3.1.0
        # {
        #   name = "fish-abbreviation-tips";
        #   src = niv sources.fish-abbreviation-tips;
        # }

        # needed to source bash
        {
             name="foreign-env";
             src = niv sources.plugin-foreign-env;
        }

        {
          name = "fish-prompt-mono";
          src = niv sources.mono;
        }

        {
          name = "fzf";
          src = niv sources.fzf;
        }

        # makes !! be the last command used as in bash
        {
          name = "plugin-bang-bang";
          src = niv sources.plugin-bang-bang;
        }

      ];

      shellAbbrs = {
        gbv = "git branch --verbose";
        gco = "git checkout";
        gst = "git status";
      };

    };

    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/release-20.03.tar.gz;
    };

  };

  xdg.configFile."fish/conf.d/plugin-bobthefish.fish".text = pkgs.lib.mkAfter ''
    for f in $plugin_dir/*.fish
      source $f
    end
  '';

}
