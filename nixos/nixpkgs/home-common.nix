{ config, pkgs, ... }:
let
  niv = source: fetchTarball { inherit (source) url sha256; };
  nur = pkgs.callPackage (niv sources.NUR) { };
  pkgs = import (niv sources.nixpkgs) {
    config = { allowUnfree = true; };
    overlays = [ (import (niv sources.emacs-overlay)) ];
  };
  sources = import ./nix/sources.nix { };
in {

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "val";
  home.homeDirectory = "/Users/val";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home = {

    packages = [
      pkgs.any-nix-shell
      pkgs.bat
      pkgs.cachix
      pkgs.capstone

      # This interferes poorly with MacOS, for instance, the `cp` command won't support `-c`:
      # pkgs.coreutils

      pkgs.entr
      pkgs.expect
      pkgs.fzf
      pkgs.difftastic
      pkgs.gitAndTools.delta
      pkgs.gitAndTools.git-delete-merged-branches
      pkgs.github-cli
      pkgs.gnumake
      pkgs.graphviz
      pkgs.htop
      pkgs.nixfmt
      pkgs.niv
      pkgs.jq
      pkgs.less
      pkgs.mosh
      pkgs.tree
      pkgs.wget
      pkgs.yq
    ];

    sessionPath = [
      "$HOME/.cargo/bin"
      "$HOME/.ghcup/bin"
      "$HOME/.local/bin"
      "$HOME/.opam/default/bin"
      "/opt/homebrew/bin"
      "/opt/homebrew/opt/binutils/bin"
      "/opt/homebrew/opt/llvm@12/bin"
    ];

    sessionVariables = {
      LS_COLORS = "ow=01;34";
      HOMEBREW_PREFIX = "/opt/homebrew";
      HOMEBREW_CELLAR = "/opt/homebrew/Cellar";
      HOMEBREW_REPOSITORY = "/opt/homebrew";
      PATH = "/opt/homebrew/bin:/opt/homebrew/sbin:$PATH";
      MANPATH = "/opt/homebrew/share/man:$MANPATH";
      INFOPATH = "/opt/homebrew/share/info:$INFOPATH";
    };
  };

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/val/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  nix = {
    package = pkgs.nix;
    settings.experimental-features = [ "nix-command" "flakes" ];
  };

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

    # direnv-nix-lorelei.enable = true;

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

        export HOMEBREW_PREFIX="/opt/homebrew"
        export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
        set -gx HOMEBREW_REPOSITORY "/opt/homebrew";
        set -q PATH; or set PATH ""; set -gx PATH "/opt/homebrew/bin" "/opt/homebrew/sbin" $PATH;
        set -q MANPATH; or set MANPATH ""; set -gx MANPATH "/opt/homebrew/share/man" $MANPATH;
        set -q INFOPATH; or set INFOPATH ""; set -gx INFOPATH "/opt/homebrew/share/info" $INFOPATH;

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

      shellAliases = { ls = "ls --color=always"; };

    };

    git = {
      enable = true;
      # delta = {
      #   enable = true;
      # };
      diff-so-fancy = {
        enable = true;
      };
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

    ripgrep = {
      arguments = [ "--max-columns-preview" "--colors=line:style:bold" ];
      enable = true;
    };

    zoxide = {
      enable = true;
      enableFishIntegration = true;
    };

  };

}
