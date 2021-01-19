let

  configuration = import ./configuration.nix;
  pkgs = configuration.pkgs;

in

{
  imports = [ (import ./home-common.nix { inherit configuration; }) ];

  home = {
    packages = [
      pkgs.vscode
    ];
  };

}
