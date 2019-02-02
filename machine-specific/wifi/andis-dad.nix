{ config, pkgs, ... }:
{
  networking.wireless.networks."FiOS-GSLSG" = {
    pskRaw = "e2fbc10eb1eb6898d8b8063d5adc1fdccdc9246f495c4572d48cc23b027e47f";
  };
}
