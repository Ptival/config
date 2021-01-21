{ configuration }:

let

  niv = configuration.niv;
  nur = configuration.nur;
  pkgs = configuration.pkgs;
  sources = configuration.sources;

  # doom-emacs = pkgs.callPackage (niv sources.nix-doom-emacs) {
  #   doomPrivateDir = ../dotfiles/doom.d;
  #   emacsPackages = (pkgs.emacsPackagesNgGen
  #     (pkgs.emacsGit.override {
  #       inherit (pkgs) imagemagick;
  #       withGTK3 = true;
  #       withXwidgets = true;
  #     }));
  #   extraPackages = epkgs: [ pkgs.emacsPackages.proofgeneral_HEAD ];
  # };

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

  manual.manpages.enable = false;

  home = {

    packages = with pkgs; [
      bat               # Nicer cat
      binutils
      cabal2nix
      cachix
      dejavu_fonts
      # doom-emacs
      # emacs-all-the-icons-fonts
      fasd
      fd                # Makes file search faster in doom-emacs
      fontconfig
      fzf
      gitAndTools.delta # Nicer pager, is not automatically installed when git.delta.enable is true
      github-cli
      gnumake
      htop              # Nicer top
      iosevkass09
      jq                # JSON viewer
      less              # Better than busybox's less
      lorri
      mesloNerdP10k
      noto-fonts-emoji
      nixfmt            # Formatter for nix code
      pkgs.niv
      openssl
      # (import ./texlive.nix {})
      ripgrep           # Better grep
      # vscode          # UNWANTED on WSL2 machines, put it it machine-specific nix files
      wget
      yq                # YAML viewer
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
      # package = pkgs.fish;

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

        {
          name = "nix-env.fish";
          src = niv sources."nix-env.fish";
        }

        # Disabled while I figure out why it complains...
        # {
        #   name = "fasd";
        #   src = niv sources.plugin-fasd;
        # }

        # needs fish >= 3.1.0
        {
          name = "fish-abbreviation-tips";
          src = niv sources.fish-abbreviation-tips;
        }

        # {
        #   name = "hydro";
        #   src = niv sources.hydro;
        # }

        {
          name = "fzf";
          src = niv sources.fzf;
        }

        # makes !! be the last command used as in bash
        {
          name = "plugin-bang-bang";
          src = niv sources.plugin-bang-bang;
        }

        {
          name = "plugin-foreign-env";
          src = niv sources.plugin-foreign-env;
        }

      ];

      shellAbbrs = {
        gbv = "git branch --verbose";
        gco = "git checkout";
        gst = "git status";
      };

    };

    git = {
      enable = true;
      delta = {
        enable = true;
      };
      userName = "Valentin Robert";
    };

    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };

  };

  xdg.configFile."fish/conf.d/plugin-bobthefish.fish".text = pkgs.lib.mkAfter ''
    for f in $plugin_dir/*.fish
      source $f
    end
  '';

}
