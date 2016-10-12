{ config, lib, pkgs, ...} :

{

  imports = [
    ./emacs.nix
  ];

  boot = {
    initrd.checkJournalingFS = false;
    loader.grub.device = "/dev/sda";
  };

  environment = {
    systemPackages = with pkgs; [

      # THESE ARE ALWAYS GOOD TO HAVE
      binutils
      dmenu
      gcc
      gitAndTools.gitFull
      gnumake
      vim
      xfce.terminal

      # THESE CAN BE COMMENTED OUT WHEN BISECTING
      baobab
      cabal2nix
      chromium
      colordiff
      evince
      feh
      firefox
      gitg
      htop
      imagemagick
      m4
      mosh
      psmisc # killall command
      rlwrap
      (texlive.combine { inherit (texlive) scheme-full metafont; })
      unzip # opam needs it
      wget
      xclip

    ];
    variables = {
      EDITOR = "vim";
      SUDO_EDITOR = "vim";
    };
  };

  fileSystems = [
    {
      mountPoint = "/";
      label = "nixos";
    }
  ];

  fonts = {
    fonts = with pkgs; [
      emojione
      fira
      fira-code
      fira-mono
      symbola
      unifont
    ];
  };

  networking = {
    hostName = "onyx";
  };

  nixpkgs = {
    config.allowUnfree = true;
  };

  programs = {
    zsh.enable = true;
  };

  services = {
    openssh.enable = true;
    xserver = {
      enable = true;
      layout = "us";
      desktopManager = {
        default = "none";
        xfce.enable = true;
        xterm.enable = false;
      };
      displayManager = {
        lightdm.enable = false;
        sessionCommands = ''
          setxkbmap -option compose:ralt
          export GTK_IM_MODULE=xim
          export XCOMPOSEFILE = "/home/ptival/.XCompose"
        '';
        slim.enable = true;
      };
      windowManager = {
        default = "xmonad";
        xmonad.enable = true;
        xmonad.enableContribAndExtras = true;
        xmonad.extraPackages = haskellPackages: with haskellPackages; [
          xmonad-contrib
          xmonad-extras
        ];
      };
    };
  };

  time = {
    timeZone = "America/Los_Angeles";
  };

  users = {
    mutableUsers = false;
    extraUsers.ptival = {
      isNormalUser = true;
      home = "/home/ptival";
      description = "Valentin Robert";
      extraGroups = [ "tty" "wheel" ];
      hashedPassword = "$6$ISRUIiRHTmnpeO5P$CC462xIJS05eltVpeo7rZ2nIFK4Xy1XpNtc72jKKYLTqi7B8O1v2ufcr7mwxfletpd03tAXapp2WpENC5L3ib0";
      shell = "/run/current-system/sw/bin/zsh";
    };
  };

  virtualisation = {
    virtualbox.guest.enable = true;
  };

}
