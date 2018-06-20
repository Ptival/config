# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./machine-specific.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  programs = {
    zsh.enable = true;
  };

  environment.variables = {
    EDITOR = "vim";
    SUDO_EDITOR = "vim";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    binutils
    dmenu
    emacs
    gcc
    gitAndTools.gitFull
    gnumake
    nix-repl
    vim
    xfce.terminal
    wget

    aspell
    aspellDicts.en
    aspellDicts.fr
    bashInteractive
    baobab
    cabal2nix
    chromium
    colordiff
    coq
    evince
    feh
    file
    #firefox
    #gitg
    graphviz
    #haskellPackages.ghc-mod
    haskellPackages.xmobar
    htop
    imagemagick
    lolcat
    #m4                       # opam needs this sometimes
    mosh
    #nodePackages.node2nix    # seems kinda useless at the moment
    openssl
    psmisc                    # killall command
    python36Packages.pygments
    rlwrap
    #rxvt_unicode
    slack
    spotify
    stack
    (texlive.combine {
      inherit (texlive)

      #scheme-minimal # plain
      #scheme-basic   # + latex
      #scheme-small   # + xetex
      scheme-medium  # + packages
      #scheme-full    # + more packages

      adjustbox
      algorithm2e
      anyfontsize
      babel
      babel-greek
      blindtext
      booktabs
      boondox
      bussproofs
      caption
      cbfonts
      ccicons
      cleveref
      cmap
      collectbox
      collection-fontsrecommended
      collection-pictures
      comment
      currvita
      dejavu
      doublestroke
      draftwatermark
      enumitem
      environ
      etoolbox
      euenc
      everypage
      filehook
      float
      fontaxes
      fontspec
      framed         # needed by minted
      fvextra        # needed by minted
      gfsartemisia
      gfsbaskerville
      gfsdidot
      gfsneohellenic
      greek-fontenc
      greektex
      ifplatform     # needed by minted
      inconsolata
      latexmk
      libertine
      listings
      mathpartir
      mdwtools
      metafont
      microtype
      minted
      ms
      mweights
      ncctools
      newtx
      relsize
      sfmath
      soul
      stmaryrd
      textcase
      titlesec
      tocloft
      totpages
      trimspaces
      ucs
      upquote
      xcolor
      xetex
      #xetex-def
      xstring
      ;
    })
    unzip # opam needs it
    #vscode
    xclip

  ];

  fonts = {
    #fontconfig = {
    #  enable = true;
    #  includeUserConf = true;
    #};
    fonts = with pkgs; [
      corefonts
      emacs-all-the-icons-fonts
      dejavu_fonts
      #emojione
      #fira
      #fira-code
      #fira-code-symbols
      #fira-mono
      #font-awesome-ttf
      helvetica-neue-lt-std
      #freefont_ttf
      #hasklig
      #input-fonts
      #nerdfonts
      #symbola
      #unifont
    ];
  };

  nix.nixPath = [
    "nixpkgs=/home/ptival/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager = {
    lightdm.enable = true;
    sessionCommands = ''
      #xmodmap -e "keysym Super_L = Multi_key"
      setxkbmap -option compose:ralt
      setxkbmap -option compose:prsc # for my Kinesis keyboard, no AltGr
      export GTK_IM_MODULE=xim
      export XCOMPOSEFILE = "/home/ptival/.XCompose"
    '';
  };
  services.xserver.desktopManager = {
    default = "xfce";
    xfce = {
      enable = true;
      enableXfwm = false;
    };
  };
  services.xserver.windowManager = {
    default = "xmonad";
    xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: with haskellPackages; [
        xmonad-contrib
        xmonad-extras
      ];
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  users = {
    mutableUsers = false;
    extraUsers.ptival = {
      isNormalUser = true;
      home = "/home/ptival";
      description = "Valentin Robert";
      extraGroups = [ "tty" "wheel" ];
      hashedPassword = "$6$ISRUIiRHTmnpeO5P$CC462xIJS05eltVpeo7rZ2nIFK4Xy1XpNtc72jKKYLTqi7B8O1v2ufcr7mwxfletpd03tAXapp2WpENC5L3ib0";
      shell = pkgs.zsh;
    };
  };

 system = {
    copySystemConfiguration = true;
    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
     stateVersion = "18.03"; # Did you read the comment?
  };

}
