{ nixpkgs ? import <nixpkgs> {}
}:
nixpkgs.mkShell {

  buildInputs = [
    nixpkgs.cargo
    nixpkgs.libiconv
    nixpkgs.darwin.apple_sdk.frameworks.CoreServices
  ];

  name = "rust-analyzer";

}
