let

  sources = import ./nix/sources.nix {};
  fetchNiv = niv: fetchTarball { inherit (niv) url sha256; };

  pkgs = import (fetchNiv sources.nixpkgs) {
    config = {
      allowUnfree = true;
    };
    overlays = [
      (import (fetchNiv sources.emacs-overlay))
    ];
  };
  homeManager = fetchNiv sources.home-manager;
  nur = pkgs.callPackage (fetchNiv sources.NUR) {};

  userName = "val";

  doom-emacs = pkgs.callPackage (fetchNiv sources.nix-doom-emacs) {
    doomPrivateDir = ./dotfiles/doom.d;
    emacsPackages = (pkgs.emacsPackagesNgGen
      (pkgs.emacsGit.override {
        inherit (pkgs) imagemagick;
        withGTK3 = true;
        withXwidgets = true;
      }));
    extraPackages = epkgs: [ pkgs.emacsPackages.proofgeneral_HEAD ];
  };

in
{

  console = {
    keyMap = "us";
  };

  environment.systemPackages = with pkgs; [
    adapta-kde-theme
    baobab            # Disk space usage viewer
    bat               # Nicer cat
    binutils
    cabal2nix
    cachix
    direnv
    discord
    doom-emacs
    fd                # Makes doom-emacs file search faster
    feh               # Lightweight image viewer
    firefox
    fzf-zsh # Fuzzy line-finder for zsh
    gimp
    git
    gitAndTools.delta # Nicer pager
    gnumake
    htop              # Nicer top
    jq                # Jquery viewer
    less              # Better than busybox's less
    nixfmt            # Code formatter for nix
    niv
    openssl
    ripgrep           # Nicer grep
    slack
    spectacle
    spotify
    terminator        # Nice terminal
    # (import ./texlive.nix {})
    wget
    yq                # Yaml viewer
    wget
    zsh-powerlevel10k
  ];

  fonts = {
    enableDefaultFonts = true;
    fontconfig = {
      defaultFonts.emoji = [ "Noto Color Emoji" ];
    };
    fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      (iosevka.override {
        privateBuildPlan = {
          design = [ "ss09" ];
          family = "Iosevka SS09";
        };
        set = "ss09";
      })
      noto-fonts-emoji
      nur.repos.ptival.meslo-nerd-powerlevel10k
      # symbola
    ];
  };

  # Use the nixpkgs set by nixpkgs here
  home-manager.useGlobalPkgs = true;
  home-manager.users.${userName} =
    { config, lib, pkgs, ... }:
    {

      home = {
        packages = with pkgs; [
        ];
      };

      # NOTE: this does not exist when `useGlobalPkgs` is set
      # nixpkgs.config = {
      #   allowUnfree = true;
      # };

    };

  i18n.defaultLocale = "en_US.UTF-8";

  imports =
    [
      "${homeManager}/nixos"
      ./cachix.nix
      # ./emacs.nix
      ./hardware-configuration.nix
      ./machine-specific.nix
      # This is for VSCode LiveShare feature
      # "${builtins.fetchGit {
      #   url = "https://github.com/msteen/nixos-vsliveshare.git";
      #   ref = "refs/heads/master";
      # }}"
    ];

  nixpkgs = {

    # config = {
    #   allowUnfree = true;
    # };

    overlays = [
      # NOTE: If you want overlays to already be active in this file, you should put
      # them up there where `pkgs` is defined.
    ];

    inherit pkgs;

  };

  programs = {
    vim.defaultEditor = true;

    zsh = {
      autosuggestions.enable = true;
      enable = true;

      interactiveShellInit = ''
export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/ # oh-my-zsh: use nix store version
plugins=(                                     # oh-my-zsh: plugins to load
  git
                             )
source $ZSH/oh-my-zsh.sh                      # oh-my-zsh: load
bindkey -e                                    # zsh:       use emacs keybindings
source ~/.common.rc.sh                        # zsh:       source aliases
eval "$(direnv hook zsh)"                     # zsh:       use direnv

# The following makes it so that <Shift-Tab> forces file completion regardless of context
# It is useful because sometimes zsh will refuse to complete, for instance:
# - for git commands, it won't complete ignored files
# - for cabal, it will not complete any file!!!
zle -C complete complete-word complete-files
bindkey '^[[Z' complete
complete-files () { compadd - $PREFIX* }
      '';

      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
      };

      promptInit = ''
source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
       '';

      syntaxHighlighting = {
        enable = true;
        highlighters = [ "main" "brackets" "root" ];
      };

    };
  };

  services = {

    lorri.enable = true;

    # vsliveshare = {
    #   enable = true;
    #   extensionsDir = "/home/val/.vscode/extensions";
    # };

    xserver = {
      enable = true;
      layout = "us";

      desktopManager.plasma5.enable = true;

      displayManager = {
        defaultSession = "plasma5";
        # defaultSession = "plasma5+xmonad";
        sddm.enable = true;
        # TODO: separate this by machine
        sessionCommands = ''

          # turns CapsLock into Ctrl
          setxkbmap -option ctrl:nocaps

          # xmodmap -e "keysym Super_L = Multi_key"

          # use the right Alt key as XCompose key
          setxkbmap -option compose:ralt # RightAlt is a XCompose key

          # sadly some keyboards still send the Alt_R signal and the previous
          # trick does not work, so this *really* turns the right alt key into
          # the multi key for XCompose
          xmodmap -e "keysym Alt_R = Multi_key"
          xmodmap -e "keysym Caps_Lock = Control_L"

          #setxkbmap -option compose:prsc # for my Kinesis keyboard, no AltGr

          # other options needed for XCompose to kick in
          export GTK_IM_MODULE=xim
          export XCOMPOSEFILE = "/home/${userName}/.XCompose"
          export XDG_CONFIG_HOME = "/home/${userName}/.config"


        '';
      };

      windowManager = {
        # xmonad = {
        #   enable = true;
        #   enableContribAndExtras = true;
        # };
      };

    };
  };

  users = {
    mutableUsers = false;
    extraUsers.${userName} = {
      isNormalUser = true;
      home = "/home/${userName}";
      description = "Valentin Robert";
      # Extra groups may be added in machine-specific/<machine>.nix
      # For machines that need wifi, add "networkmanager"
      # For machines that need printing, add "lp"
      extraGroups = [ "docker" "tty" "wheel" ];
      hashedPassword = "$6$ISRUIiRHTmnpeO5P$CC462xIJS05eltVpeo7rZ2nIFK4Xy1XpNtc72jKKYLTqi7B8O1v2ufcr7mwxfletpd03tAXapp2WpENC5L3ib0";
      shell = pkgs.zsh;
    };
  };

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
