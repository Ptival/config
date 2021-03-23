{ clang, pkgs, stdenv }:

let
  sources = import ../sources.nix {};
in

stdenv.mkDerivation rec {
  name = "macos11-haskell-workaround-${version}";
  version = sources.macos11-haskell-workaround.rev;

  src = sources.macos11-haskell-workaround;

  buildPhase = ''
    clang -target x86_64-darwin -dynamiclib macos11ghcwa.c -o macos11ghcwa.dylib
  '';

  installPhase = ''
    mkdir -p $out
    mv macos11ghcwa.dylib $out
  '';

  enableParallelBuilding = true;

  meta = {
    description = "Workaround for MacOS Big Sur dynamic linker issues";
    maintainers = [ pkgs.lib.maintainers.ptival ];
  };

}
