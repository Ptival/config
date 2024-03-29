let

  sources = import ./nix/sources.nix { };
  home-manager-sources = import ./nixpkgs/nix/sources.nix { };
  fetchNiv = niv: fetchTarball { inherit (niv) url sha256; };

  pkgs = import (fetchNiv sources.nixpkgs) {
    config = {
      allowUnfree = true;
    };
  };
  homeManager = fetchNiv home-manager-sources.home-manager;
  nur = pkgs.callPackage (fetchNiv sources.NUR) { };

  userName = "val";

in
{

  console = {
    keyMap = "us";
  };

  # Here I put packages that are relevant to a full NixOS installation,
  # where I want some GUI applications not provided by some other host.
  #
  # For general programming applications that are also relevant to
  # non-NixOS machines, I put those in ../nixpkgs/home.nix
  environment.systemPackages = with pkgs; [
    adapta-kde-theme # KDE theme
    baobab # Disk space usage viewer
    discord
    feh # Lightweight image viewer
    firefox
    gimp
    slack
    spectacle # For taking screen captures
    spotify
    terminator # Nice terminal
  ];

  # Use the nixpkgs set by nixpkgs here
  home-manager.useGlobalPkgs = true;
  home-manager.users.${userName} = import ./nixpkgs/home.nix;

  i18n.defaultLocale = "en_US.UTF-8";

  imports =
    [
      "${homeManager}/nixos"
      ./cachix.nix
      ./hardware-configuration.nix
      ./machine-specific.nix
      # This is for VSCode LiveShare feature
      # "${builtins.fetchGit {
      #   url = "https://github.com/msteen/nixos-vsliveshare.git";
      #   ref = "refs/heads/master";
      # }}"
    ];

  nix = {
    binaryCaches = [
      "http://hydra.iohk.io/"
    ];
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

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
  };

  services = {

    lorri.enable = true;

    # nextcloud.package = pkgs.nextcloud18;

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
      shell = pkgs.fish;
    };
  };

  # virtualisation.docker.enable = true;
  # virtualisation.virtualbox.host.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
