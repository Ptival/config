let
  configuration = import ./configuration.nix;
  pkgs = configuration.pkgs;
in
{

  imports = [ (import ./home-common.nix { inherit configuration pkgs; }) ];

  home = {

    packages = [
      pkgs.colima
      pkgs.docker
      pkgs.docker-compose
      # pkgs.libiconv # otherwise lots of clang problems
      (pkgs.lima.override { withAdditionalGuestAgents = true; })
      # pkgs.nodejs_latest
    ];

  };

}
