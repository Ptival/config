# Here's how to set up WPA2-Enterprise with PEAP.
# Since the password must appear in clear, do not add those files to GitHub!
# Instead, add them to .gitignore to make sure they are never added!
{ config, pkgs, ... }:
{
  networking.wireless.networks."<SSID>" = {
    auth = ''
      key_mgmt=WPA-EAP
      identity="<username>"
      password="<password>"
      eap=PEAP
      phase1="peaplabel=0"
      phase2="auth=MSCHAPV2"
    '';
  };
}
