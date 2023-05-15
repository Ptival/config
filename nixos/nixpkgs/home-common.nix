{ configuration }:

let

  niv = configuration.niv;
  nur = configuration.nur;
  pkgs = configuration.pkgs;
  sources = configuration.sources;

  # Disabled until this is fixed:
  # https://github.com/nix-community/emacs-overlay/issues/124

  doom-emacs = pkgs.callPackage (niv sources.nix-doom-emacs) {
    doomPrivateDir = ../dotfiles/doom.d;
    # emacsPackages = (pkgs.emacsPackagesNgGen
    #   (pkgs.emacsGit.override {
    #     # inherit (pkgs) imagemagick;
    #     # withGTK3 = true;
    #     # withXwidgets = true;
    #   }));
    extraPackages = epkgs: [
      pkgs.emacsPackages.proof-general
    ];
    # dependencyOverrides = {
    #   "auctex" = throw "TODO";
    # };
  };

  pkgs-bootstrap = import <nixpkgs> { config = { }; overlays = [ ]; };

  lorelei-source = pkgs-bootstrap.fetchFromGitHub {
    owner = "shajra";
    repo = "direnv-nix-lorelei";
    rev = "7342d5b95e84e4d9b885d4afa82b061c7278297c";
    sha256 = "sha256-KhLyeRqIQsa4axO6+RSMxIi8iibWefgnLcaWC+dL7Gc=";
    # sha256 = pkgs.lib.fakeSha256;
  };
  module-lorelei = (import lorelei-source).direnv-nix-lorelei-home;

  # myHomeManager = import (niv sources.home-manager) {};

  binary-ninja = nur.repos.mic92.binary-ninja;
  mesloNerdP10k = nur.repos.ptival.meslo-nerd-powerlevel10k;

in
{

  # Does not work on NixOS with useGlobalPkgs
  fonts.fontconfig.enable = true;

  manual.manpages.enable = false;

  home = rec {

    homeDirectory =
      if pkgs.stdenv.isDarwin
      then "/Users/${username}"
      else "/home/${username}";

    packages = with pkgs; [
      (pkgs.writeScriptBin "nixFlakes" ''
        #! /usr/bin/env fish
        exec ${pkgs.nixUnstable}/bin/nix --experimental-features "nix-command flakes" "$argv"
      '')
      any-nix-shell
      bat # Nicer cat
      # binary-ninja
      # binutils # Incompatible with clang?
      # cabal2nix # not currently using
      cachix
      # clang # Needed for GHC to find `ld`?
      coreutils # Makes sure we have the version of `ls` that accepts arguments like --color
      dejavu_fonts
      # doom-emacs # not currently using
      # emacs
      emacs-all-the-icons-fonts
      fasd
      fd # Makes file search faster in doom-emacs
      fira-code
      fira-mono
      fontconfig
      fzf
      difftastic
      gitAndTools.delta # Nicer pager, is not automatically installed when git.delta.enable is true
      gitAndTools.git-delete-merged-branches
      github-cli
      gnumake
      graphviz

      # EDIT: Actually it creates problems to have ghc/cabal available! :-D
      # It is nice to always have some Haskell packages available for
      # bootstrapping project, even if the projects bring their own copy of
      # such tools.
      # haskellPackages.cabal-install
      # haskellPackages.hpack
      # haskellPackages.ghc

      htop # Nicer top
      # iosevka
      jq # JSON viewer
      less # Better than busybox's less
      libertine # Font used by SIGPLAN LaTeX template
      libiconv # Needed by some Haskell packages
      lorri
      mesloNerdP10k
      mosh
      # noto-fonts-emoji
      nix-du
      nixfmt # Formatter for nix code
      nixpkgs-fmt # Other formatter?
      pkgs.niv
      opam # DO NOT REMOVE WITHOUT FIXING interactiveShellInit
      openssl
      # (import ./texlive.nix { }) # not currently using, takes forever to compile
      ripgrep # Better grep
      rustup
      # socat # Was using to do FIFO stuff on transmute
      tree
      # vscode          # UNWANTED on WSL2 machines, put it it machine-specific nix files
      vimpager
      # websocat # Was using to do FIFO stuff on crux
      wget
      yq # YAML viewer

      # The followings are necessary to install coqide via opam
      # NVM: doesn't work, installing via system package manager instead
      # cairo
      # expat
      # gnome.adwaita-icon-theme
      # gtk3
      # gtksourceview3

    ];

    sessionPath = [
      "$HOME/.ghcup/bin"
      "$HOME/.local/bin"
    ];

    sessionVariables = { LS_COLORS = "ow=01;34"; };
    stateVersion = "21.11";
    username = "val";

  };

  # NOTE: this does not exist when `useGlobalPkgs` is set
  # nixpkgs.config = {
  #   allowUnfree = true;
  # };

  imports = [ module-lorelei ];

  programs = {

    direnv = {
      enable = true;
      # enableFishIntegration = true;
      # enableZshIntegration = true;
      nix-direnv = {
        enable = true;
        # enableFlakes = true;
      };
    };

    direnv-nix-lorelei.enable = true;

    fish = {
      enable = true;
      # package = pkgs.fish;

      # Added towards the end of ~/.config/fish/config.fish
      interactiveShellInit = ''
        # Windows terminal wants to start us in C:/Users/<User>/...
        # cd ~
        # Need to find something else, because this breaks stuff like vscode debugging
        export NIX_PATH=$HOME/.nix-defexpr/channels

        any-nix-shell fish --info-right | source

        eval (opam env)
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

      shellAliases = {
        ls = "ls --color=always";
      };

    };

    git = {
      enable = true;
      # delta = {
      #   enable = true;
      # };
      lfs.enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = "Valentin Robert";
    };

    home-manager = {
      enable = true;
      path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    };

    neovim = {
      enable = true;

      extraConfig = ''
        colorscheme default
      '';

      coc = {
        enable = true;
        # Temporary workaround for bug:
        # https://github.com/nix-community/home-manager/issues/2966
        package = pkgs.vimUtils.buildVimPluginFrom2Nix {
          pname = "coc.nvim";
          version = "2022-05-21";
          src = pkgs.fetchFromGitHub {
            owner = "neoclide";
            repo = "coc.nvim";
            rev = "791c9f673b882768486450e73d8bda10e391401d";
            sha256 = "sha256-MobgwhFQ1Ld7pFknsurSFAsN5v+vGbEFojTAYD/kI9c=";
          };
          meta.homepage = "https://github.com/neoclide/coc.nvim/";
        };
      };

      viAlias = true;
      vimAlias = true;
      withNodeJs = true;
    };

    # vim = {
    #   enable = true;
    #   # extraConfig = ''
    #   #   so ~/.vim/plugin/unicode.vim
    #   #   " colorscheme delek
    #   #   " " filetype plugin indent on      " ?
    #   #   " set autoindent                 " Indent based on previous line indentation
    #   #   " " set backspace=indent,eol,start " ?
    #   #   " set expandtab                  " Pressing <TAB> inserts spaces according to 'shiftwidth' and 'softtabstop'
    #   #   " set number                     " Display line numbers
    #   #   " set softtabstop=2
    #   #   " set shiftwidth=2
    #   #   " set smartindent                " Helps autoindent make smarter, language-based choices
    #   #   " " syntax on
    #   # '';
    #   packageConfigurable = pkgs.vimHugeX;
    # };

    zsh = {
      autocd = true;
      enable = true;
      enableSyntaxHighlighting = true;
      enableVteIntegration = true;
      initExtra = ''
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
  source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
fi
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
      '';
      zplug = {
        enable = true;
        plugins = [
          { name = "coot/zsh-haskell"; tags = ["14ac0fadc9b61fd27a65be35cd0591fd5504974a"]; }
        ];
      };
    };

  };

  # This does not work on MacOS
  # services = {
  #   emacs = {
  #     enable = true;
  #     package = doom-emacs;
  #   };
  # };

  # xdg.configFile."fish/conf.d/plugin-bobthefish.fish".text = pkgs.lib.mkAfter ' ' # had to put a space between quotes to comment it out
  #   for f in $plugin_dir/*.fish
  #     source $f
  #   end
  # ' ';

}
