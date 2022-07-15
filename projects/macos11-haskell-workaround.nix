{ clang, pkgs, source, stdenv }:
stdenv.mkDerivation rec {

  name = "macos11-haskell-workaround-${version}";
  version = source.rev;

  src = source;

  buildDepends = [
    clang
  ];

  buildPhase = ''
    ${clang}/bin/clang -target x86_64-darwin -dynamiclib macos11ghcwa.c -o macos11ghcwa.dylib
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
