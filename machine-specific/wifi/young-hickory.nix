{ config, pkgs, ... }:
{
  networking.wireless.networks."Young Hickory" = {
    pskRaw = "2b0894a41ad207aaa1239d0cef4059075b1995c2f0bad1fd5756c31fce2976ba";
  };
}
