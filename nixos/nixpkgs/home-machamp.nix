let

  configuration = import ./configuration.nix;
  pkgs = configuration.pkgs;

  # This is nice but it takes way too long to build!
  # iosevkass09 = pkgs.iosevka.override {
  #   privateBuildPlan = {
  #     design = [ "ss09" ];
  #     family = "Iosevka SS09";
  #   };
  #   set = "ss09";
  # };

  vscode-insiders = ((pkgs.vscode.override { isInsiders = true; }).overrideAttrs
    (_: rec {
      pname = "vscode-insiders";
      version = "i-dont-know";

      #     src = builtins.fetchurl {
      #       name = "VSCode_latest_linux-x64.tar.gz";
      #       url =
      #         "https://vscode-update.azurewebsites.net/latest/darwin/insider";
      #     };

      src = pkgs.fetchurl {
        url =
          "https://az764295.vo.msecnd.net/insider/40d5e6796fbc32d67dd0009e5b0027003803fd7e/VSCode-darwin-universal.zip";
        sha256 = "1pvg57x0y6lnj65m404m3m4g0mr836dhwvinqdmcdgw1651ikaln";
      };

    }));

in
{

  imports = [ (import ./home-common.nix { inherit configuration; }) ];

  home = {

    packages = [
      pkgs.colima
      pkgs.docker
      pkgs.docker-compose
      pkgs.libiconv # otherwise lots of clang problems
      pkgs.nodejs_latest
      # Broken on nixpkgs-unstable
      # pkgs.python3Packages.poetry
      pkgs.vscode
      # vscode-insiders
      # iosevkass09
    ];

    sessionPath = [
      "$HOME/.cabal/bin"
      "$HOME/.ghcup/bin"
      "$HOME/.local/bin"
    ];

  };

}
