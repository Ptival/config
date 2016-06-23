{ config, pkgs, ...} :

{
  require = [
    ./ghc.nix
    ./xmonad.nix
  ];

  boot.loader.grub.device = "/dev/sda";

  environment.systemPackages = with pkgs; [
    gettext # needed momentarily because of a buggy build
    samba
  ];

  environment.variables = {
    EDITOR = "vim";
    SUDO_EDITOR = "vim";
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  virtualisation.virtualbox.guest.enable = true;

  services.openssh.enable = true;

  services.virtualbox.guest.enable = true;

  services.samba = {
    enable = true;
    shares = {
      peacoq =
        { path = "/home/ptival/PeaCoq/";
          browseable = "yes";
          "guest ok" = "yes";
          "public" = "yes";
          "read only" = "no";
        };
    };
    extraConfig = ''
    guest account = ptival
    map to guest = bad user
    security = user
    workgroup = WORKGROUP
    '';
  };

  fileSystems = [
    {
      mountPoint = "/";
      label = "nixos";
    }
    {
      mountPoint = "/vboxshare";
      fsType = "vboxsf";
      device = "Shared";
      options = ["rw"];
    }
  ];

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "devicemapper";

  fonts.fonts = [ pkgs.fira pkgs.fira-code pkgs.fira-mono ];

  services.xserver = {
    enable = true;
    layout = "us";
    displayManager.lightdm.enable = true;
  };

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
  services.locate.enable = true;

  users.mutableUsers = false;
  users.extraUsers.ptival =
    { isNormalUser = true;
      home = "/home/ptival";
      description = "Valentin Robert";
      extraGroups = [ "docker" "wheel" ];
      hashedPassword = "$6$ISRUIiRHTmnpeO5P$CC462xIJS05eltVpeo7rZ2nIFK4Xy1XpNtc72jKKYLTqi7B8O1v2ufcr7mwxfletpd03tAXapp2WpENC5L3ib0";
    };

  time.timeZone = "America/Los_Angeles";

}

