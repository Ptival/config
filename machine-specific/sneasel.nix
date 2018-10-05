{ config, pkgs, ... }:
{
  imports =
    [
      ./wifi/ariel.nix
      ./wifi/better-buzz-pacific-beach.nix
      ./wifi/dimos.nix
      ./wifi/johns-appt-san-francisco.nix
      ./wifi/krakatoa.nix
      ./wifi/philz.nix
      ./wifi/pistachio.nix
      ./wifi/rock-bottom.nix
      ./wifi/subterranean.nix
      ./wifi/toma-sol.nix
      ./wifi/twiggs.nix
      ./wifi/wifi.nix
      ./wifi/working-class.nix
      ./wifi/young-hickory.nix

      # I don't think WPA2-E supports pre-encrypted, so key is in clear :$
      ./wifi/private/ucsd-protected.nix
    ];

  networking = {
    hostName = "sneasel";
    nameservers = [ "8.8.8.8" "192.168.86.1" ];
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

  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;
  services.xserver.videoDrivers = [ "opengl" ];
}
