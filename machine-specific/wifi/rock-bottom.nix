{ config, pkgs, ... }:
{
  networking.wireless.networks."Rock Bottom - Guest" = {
    pskRaw = "fceebcbcf3998635b24eecea67188783f3e90555bce73b847c25c05aa4a1d388";
  };
}
