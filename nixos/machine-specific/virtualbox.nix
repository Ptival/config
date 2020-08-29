{ config, lib, pkgs, ...} :

{

  boot = {
    initrd.checkJournalingFS = false;
    loader.grub.device = "/dev/sda";
  };

  networking = {
    hostName = "onyx";
  };

  virtualisation = {
    virtualbox.guest.enable = true;
  };

}
