# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./emacs.nix # Adds ProofGeneral
      ./hardware-configuration.nix # Include the results of the hardware scan.
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
    gcc
    gitAndTools.gitFull
    gnumake
    libreoffice-still
    vim
    wget
    wpa_supplicant
    wpa_supplicant_gui
    xfce.terminal
    xfce.xfce4-battery-plugin
    xfce.xfce4-cpugraph-plugin
    xfce.xfce4-datetime-plugin
    xfce.xfce4-hardware-monitor-plugin
    xfce.xfce4-icon-theme
    xfce.xfce4_power_manager_gtk3
    xfce.xfce4-screenshooter

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
    firefox
    #gitg
    gimp
    glxinfo
    graphviz
    #haskellPackages.ghc-mod
    haskellPackages.xmobar
    htop
    imagemagick
    lolcat
    #m4                       # opam needs this sometimes
    mosh
    nodejs
    #nodePackages.node2nix    # seems kinda useless at the moment
    openssl
    playonlinux
    psmisc                    # killall command
    python36Packages.pygments
    rlwrap
    #rxvt_unicode
    slack
    spotify
    stack
    steam
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
    vscode
    wine
    winetricks
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
      #emojione # REMOVED: interferes with Noto
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
      noto-fonts-emoji
      #symbola
      tex-gyre.pagella
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
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint pkgs.hplip ];

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
      setxkbmap -option ctrl:nocaps  # turn CapsLock into Ctrl
      #xmodmap -e "keysym Super_L = Multi_key"
      setxkbmap -option compose:ralt # RightAlt is a XCompose key
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
      extraGroups = [ "lp" "tty" "wheel" ];
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
