{ configuration }:

let

  niv = configuration.niv;
  nur = configuration.nur;
  pkgs = configuration.pkgs;
  sources = configuration.sources;

  # Disabled until this is fixed:
  # https://github.com/nix-community/emacs-overlay/issues/124

  # doom-emacs = pkgs.callPackage (niv sources.nix-doom-emacs) {
  #   doomPrivateDir = ../dotfiles/doom.d;
  #   # emacsPackages = (pkgs.emacsPackagesNgGen
  #   #   (pkgs.emacsGit.override {
  #   #     # inherit (pkgs) imagemagick;
  #   #     # withGTK3 = true;
  #   #     # withXwidgets = true;
  #   #   }));
  #   extraPackages = epkgs: [
  #     pkgs.emacsPackages.proofgeneral_HEAD
  #   ];
  #   dependencyOverrides = {
  #     "auctex" = throw "TODO";
  #   };
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

    # file.".emacs.d/init.el".text = ''
    #   (load "default.el")
    # '';

    packages = with pkgs; [
      any-nix-shell
      bat               # Nicer cat
      binutils
      cabal2nix
      cachix
      dejavu_fonts
      # doom-emacs
      # emacs
      # emacs-all-the-icons-fonts
      fasd
      fd                # Makes file search faster in doom-emacs
      fontconfig
      fzf
      gitAndTools.delta # Nicer pager, is not automatically installed when git.delta.enable is true
      gitAndTools.git-delete-merged-branches
      github-cli
      gnumake

      # It is nice to always have some Haskell packages available for
      # bootstrapping project, even if the projects bring their own copy of
      # such tools.
      haskellPackages.cabal-install
      haskellPackages.hpack
      haskellPackages.ghc

      htop              # Nicer top
      iosevkass09
      jq                # JSON viewer
      less              # Better than busybox's less
      lorri
      mesloNerdP10k
      noto-fonts-emoji
      nixfmt            # Formatter for nix code
      nixpkgs-fmt       # Other formatter?
      pkgs.niv
      openssl
      # (import ./texlive.nix {})
      ripgrep           # Better grep
      # vscode          # UNWANTED on WSL2 machines, put it it machine-specific nix files
      vimpager
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

      # Added towards the end of ~/.config/fish/config.fish
      interactiveShellInit = ''
      # Windows terminal wants to start us in C:/Users/<User>/...
      # cd ~
      # Need to find something else, because this breaks stuff like vscode debugging

      any-nix-shell fish --info-right | source
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
      # delta = {
      #   enable = true;
      # };
      package = pkgs.gitAndTools.gitFull;
      userName = "Valentin Robert";
    };

    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };

    vim = {
      enable = true;
      extraConfig = ''
        colorscheme delek
        " filetype plugin indent on      " ?
        set autoindent                 " Indent based on previous line indentation
        " set backspace=indent,eol,start " ?
        set expandtab                  " Pressing <TAB> inserts spaces according to 'shiftwidth' and 'softtabstop'
        set number                     " Display line numbers
        set softtabstop=2
        set shiftwidth=2
        set smartindent                " Helps autoindent make smarter, language-based choices
        " syntax on
      '';
    };

  };

  xdg.configFile."fish/conf.d/plugin-bobthefish.fish".text = pkgs.lib.mkAfter ''
    for f in $plugin_dir/*.fish
      source $f
    end
  '';

}
