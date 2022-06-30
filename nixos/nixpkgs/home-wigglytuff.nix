let

  configuration = import ./configuration.nix;
  pkgs = configuration.pkgs;

  iosevkass09 = pkgs.iosevka.override {
    privateBuildPlan = {
      design = [ "ss09" ];
      family = "Iosevka SS09";
    };
    set = "ss09";
  };

in {

  imports = [ (import ./home-common.nix { inherit configuration; }) ];

  home = {

    packages = [
      # Currently does not build
      # iosevkass09
      pkgs.iosevka
      pkgs.opam
      # extra packages
    ];

    sessionPath = [
      "$HOME/.cabal/bin"
      "$HOME/.ghcup/bin"
      "$HOME/.local/bin"
    ];

  };

  programs.fish.interactiveShellInit = ''
    export NIX_PATH=$HOME/.nix-defexpr/channels
    cd ~
  '';

}
