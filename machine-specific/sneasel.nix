{ config, pkgs, ... }:
{
  networking = {
    hostName = "sneasel";
    wicd.enable = true;
    wireless = {
      enable = true;
      interfaces = [ "wlp4s0" ];
      userControlled.enable = true;
    };
  };
}
