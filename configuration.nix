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
      nix-repl
      vim
      xfce.terminal

      # THESE CAN BE COMMENTED OUT WHEN BISECTING
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
      #feh
      file
      #firefox
      #gitg
      graphviz
      #haskellPackages.ghc-mod
      haskellPackages.xmobar
      #hugs
      htop
      imagemagick
      lolcat
      m4 # opam needs this sometimes
      mosh
      #nodePackages.node2nix # seems kinda useless at the moment
      openssl
      psmisc # killall command
      python36Packages.pygments
      rlwrap
      #rxvt_unicode
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
      virtualbox
      #vscode
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
    #fontconfig = {
    #  enable = true;
    #  includeUserConf = true;
    #};
    fonts = with pkgs; [
      corefonts
      emacs-all-the-icons-fonts
      dejavu_fonts
      emojione
      fira
      fira-code
      fira-code-symbols
      fira-mono
      font-awesome-ttf
      helvetica-neue-lt-std
      freefont_ttf
      hasklig
      #input-fonts
      nerdfonts
      #symbola
      #unifont
    ];
  };

  networking = {
    hostName = "onyx";
  };

  nix.nixPath = [
    "nixpkgs=/home/ptival/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  nixpkgs.config = {
    allowUnfree = true;
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
          #xmodmap -e "keysym Super_L = Multi_key"
          setxkbmap -option compose:ralt
          setxkbmap -option compose:prsc # for my Kinesis keyboard, no AltGr
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
      shell = pkgs.zsh;
    };
  };

  virtualisation = {
    virtualbox.guest.enable = true;
  };

}
