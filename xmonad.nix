# xmonad desktop config
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    haskellPackages.ghc
    dmenu
    feh
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    scrot # for screenshot
  ];

  services.xserver = {
    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
      default = "xmonad";
    };

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
  };
}
