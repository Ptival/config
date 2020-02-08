# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{

  console = {
    keyMap = "us";
  };

  environment.systemPackages = with pkgs; [
    binutils
    direnv
    dmenu
    gcc
    gitAndTools.gitFull
    gnumake
    # iterm2
    vim
    wget
    # wpa_supplicant
    # wpa_supplicant_gui
    xfce.terminal
    xfce.xfce4-battery-plugin
    xfce.xfce4-cpugraph-plugin
    xfce.xfce4-datetime-plugin
    xfce.xfce4-hardware-monitor-plugin
    xfce.xfce4-icon-theme
    xfce.xfce4_power_manager_gtk3
    xfce.xfce4-screenshooter
    zsh-powerlevel10k

    cabal2nix
    firefox
    lorri
    ripgrep
    spotify
  ];

  fonts = {
    fontconfig = {
      defaultFonts.emoji = [ "Noto Color Emoji" ];
    #  enable = true;
    #  includeUserConf = true;
    };
    fonts = with pkgs; [
      #corefonts
      # emacs-all-the-icons-fonts
      # dejavu_fonts
      #emojione # REMOVED: interferes with Noto
      #fira
      #fira-code
      #fira-code-symbols
      #fira-mono
      #font-awesome_4
      # font-awesome_5
      #helvetica-neue-lt-std
      #freefont_ttf
      #hasklig
      #input-fonts
      (iosevka.override {
        privateBuildPlan = {
          design = [ "ss05" ];
          family = "Iosevka SS05";
        };
        set = "ss05";
      })
      # nerdfonts
      #noto-fonts-emoji
      # powerline-fonts
      #symbola
      #tex-gyre.pagella
      #unifont
    ];
  };

  i18n.defaultLocale = "en_US.UTF-8";

  imports =
    [
      ./emacs.nix
      ./hardware-configuration.nix
      ./machine-specific.nix
    ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
        };
      };
    };
  };

  programs = {
    zsh = {
      enable = true;

      interactiveShellInit = ''
export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/ # oh-my-zsh: use nix store version
plugins=(git)                                 # oh-my-zsh: plugins to load
source $ZSH/oh-my-zsh.sh                      # oh-my-zsh: load
bindkey -e                                    # zsh:       use emacs keybindings
source ~/.common.rc.sh                        # zsh:       source aliases
eval "$(direnv hook zsh)"                     # zsh:       use direnv
      '';

      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
      };

      promptInit = ''
source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
       '';
    };
  };

  services = {

    lorri.enable = true;

    xserver = {
      enable = true;
      layout = "us";

      desktopManager.plasma5.enable = true;

      displayManager = {
        defaultSession = "plasma5+xmonad";
        #gdm.enable = true;
        #lightdm.enable = true;
        sddm.enable = true;
        sessionCommands = ''
          setxkbmap -option ctrl:nocaps  # turn CapsLock into Ctrl
          #xmodmap -e "keysym Super_L = Multi_key"
          setxkbmap -option compose:ralt # RightAlt is a XCompose key
          #setxkbmap -option compose:prsc # for my Kinesis keyboard, no AltGr
          export GTK_IM_MODULE=xim
          export XCOMPOSEFILE = "/home/val/.XCompose"
        '';
      };

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };

    };
  };

  users = {
    mutableUsers = false;
    extraUsers.val = {
      isNormalUser = true;
      home = "/home/val";
      description = "Valentin Robert";
      # Extra groups may be added in machine-specific/<machine>.nix
      # For machines that need wifi, add "networkmanager"
      # For machines that need printing, add "lp"
      extraGroups = [ "tty" "wheel" ];
      hashedPassword = "$6$ISRUIiRHTmnpeO5P$CC462xIJS05eltVpeo7rZ2nIFK4Xy1XpNtc72jKKYLTqi7B8O1v2ufcr7mwxfletpd03tAXapp2WpENC5L3ib0";
      shell = pkgs.zsh;
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
