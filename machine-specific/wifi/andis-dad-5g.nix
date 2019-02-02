{ config, pkgs, ... }:
{
  networking.wireless.networks."FiOS-GSLSG-5G" = {
    pskRaw = "02ab636c03f364b0bc792fa6ebb173c53fc0aec37963dee5d1be44203dd6de30";
  };
}
