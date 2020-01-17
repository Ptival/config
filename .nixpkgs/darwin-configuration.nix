{ config, pkgs, ... }:
let
  lorri = (import (fetchGit "https://github.com/target/lorri.git") { inherit pkgs; });
in
{
  environment = {
    systemPackages = [ pkgs.direnv lorri ];
  };

  launchd.user.agents.lorri = {
    serviceConfig.ProgramArguments = [
      "${lorri}/bin/lorri" "daemon"
    ];
    serviceConfig.StandardOutPath = "/var/tmp/lorri-out.log";
    serviceConfig.StandardErrorPath = "/var/tmp/lorri-err.log";
    serviceConfig.EnvironmentVariables = {
      PATH="${pkgs.nix}/bin";
      NIX_PATH="/nix/var/nix/profiles/per-user/val/channels";
    };
    serviceConfig.Sockets = [{
      SockPathName = "/Users/val/Library/Caches/com.github.target.lorri.lorri.lorri/daemon.socket";
    }];
  };

  programs.zsh.enable = true;
}
