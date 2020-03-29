# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in

{

  console = {
    keyMap = "us";
  };

  environment.systemPackages = with pkgs; [
    adapta-kde-theme
    (all-hies.selection { selector = p: { inherit (p) ghc865 ghc882; }; })
    arc-kde-theme
    binutils
    busybox # unzip and such
    cabal2nix
    cachix
    gitAndTools.delta
    direnv
    discord
    (
      (emacsPackagesNgGen
        (emacsGit.override {
          inherit imagemagick;
          withGTK3 = true;
         })
      ).emacsWithPackages (epkgs: [
        emacsPackages.proofgeneral_HEAD
      ])
    )
    fd # makes doom-emacs file search faster
    firefox
    fzf-zsh
    gcc
    gimp
    gitAndTools.gitFull
    gnumake
    htop
    ktimetracker
    latte-dock
    lorri
    materia-theme
    obs-studio
    ripgrep
    slack
    spectacle
    spotify
    terminator
    vim
    vscode
    # NOTE: vscode-with-extensions is still very problematic with many
    # extensions that try and alter the settings directory, which is read-only
    # (vscode-with-extensions.override {
    #   vscodeExtensions = with vscode-extensions; [
    #     # Some extensions are already packaged
    #     bbenoist.Nix
    #  ] ++ vscode-utils.extensionsFromVscodeMarketplace [
    #    # Other extensions can be manually declared, e.g.
    #    {
    #      name = "vsliveshare";
    #      publisher = "MS-vsliveshare";
    #      version = "1.0.1653";
    #      sha256 = "0hasf85a7wil3npm8gk1yw2h0snh3m8784dlm6w631k1diji8ca9";
    #    }
    #     # {
    #     #   name = "vsliveshare-pack";
    #     #   publisher = "ms-vsliveshare";
    #     #   version = "0.3.4";
    #     #   sha256 = "0svijjggycnw9iy7ziiixmcf83p45q0nzvhm0pvcm982hpi4dkra";
    #     # }
    #   ];
    # })
    wget
    zoom-us
    zsh-powerlevel10k
  ];

  fonts = {
    fontconfig = {
      defaultFonts.emoji = [ "Noto Color Emoji" ];
    };
    fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      (iosevka.override {
        privateBuildPlan = {
          design = [ "ss05" ];
          family = "Iosevka SS05";
        };
        set = "ss05";
      })
      noto-fonts-emoji
      nur.repos.ptival.meslo-nerd-powerlevel10k
    ];
  };

  i18n.defaultLocale = "en_US.UTF-8";

  imports =
    [
      /etc/nixos/cachix.nix
      # ./emacs.nix
      ./hardware-configuration.nix
      ./machine-specific.nix
      # This is for VSCode LiveShare feature
      "${builtins.fetchGit {
        url = "https://github.com/msteen/nixos-vsliveshare.git";
        ref = "refs/heads/master";
      }}"
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
    overlays = [
      (import (builtins.fetchTarball {
        # Last update: March, 17th 2020
        url = https://github.com/nix-community/emacs-overlay/archive/079d1a932754e5a15388cdeb85e94d8ae13577f8.tar.gz;
      }))
    ];
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

    # lorri.enable = true;

    vsliveshare = {
      enable = true;
      extensionsDir = "/home/val/.vscode/extensions";
    };

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
          export XCOMPOSEFILE = "/home/val/.XCompose"
          export XDG_CONFIG_HOME = "/home/val/.config"


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
