{ config, pkgs, ... }:
{
  networking.wireless.networks.BBCCustomer = {
    pskRaw = "73bf572cf0e40bf666011767ee450887522bf24ccddd71fe47100b736ab57e73";
  };
}
