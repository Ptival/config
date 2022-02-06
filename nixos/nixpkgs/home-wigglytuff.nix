let

  configuration = import ./configuration.nix;
  pkgs = configuration.pkgs;

in {

  imports = [ (import ./home-common.nix { inherit configuration; }) ];

  home = {

    packages = [
      # extra packages
    ];

  };

  programs.fish.interactiveShellInit = ''
    export NIX_PATH=$HOME/.nix-defexpr/channels
    cd ~
  '';

}
