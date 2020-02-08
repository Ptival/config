{ config, pkgs, ... }:
{

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = false;
    };
  };

  networking = {
    hostName = "incineroar";
    networkmanager.enable = true;
  };

  hardware = {
    bluetooth.enable = true;
    opengl = {
      driSupport = true;
      driSupport32Bit = true;
    };
    pulseaudio.enable = true;
  };

  time.timeZone = "Europe/Paris";

  services = {
    printing.enable = true;
    xserver = {
      libinput.enable = true; # touchpad support
      videoDrivers = [ "opengl" ];
    };
  };

  sound.enable = true;

  # Required so the user can change wifi settings
  users.extraUsers.val.extraGroups = [ "lp" "networkmanager" ];

}
