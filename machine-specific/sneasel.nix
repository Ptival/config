{ config, pkgs, ... }:
{
  imports =
    [
      ./wifi/pistachio.nix
      ./wifi/subterranean.nix
      ./wifi/ucsd-protected.nix
      ./wifi/wifi.nix
    ];

  networking = {
    hostName = "sneasel";
    wicd.enable = true;
    wireless = {
      enable = true;
      interfaces = [ "wlp4s0" ];
      # To find pskRaw, run: wpa_passphrase <SSID> "<psk>"
      networks = {
        # Now, each network is in its own file in wifi/ directory, see imports
      };
      userControlled.enable = true;
    };
  };
}
