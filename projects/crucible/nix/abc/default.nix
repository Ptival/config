{ cmake, pkgs, readline, stdenv }:

let
  sources = import ../sources.nix {};
in

stdenv.mkDerivation rec {
  name = "abc-${version}";
  version = sources.abc.rev;

  # if we ever want to use upstream: check abc-verifier in nixpkgs for recent mirror
  src = sources.abc;

  # n.b. the following are documented, but libabc.so is not a valid make target:
  # ABC_USE_PIC=1 libabc.so

  nativeBuildInputs = [ cmake ];

  buildInputs = [ readline ];

  installPhase = ''
    mkdir -p $out/lib
    mv libabc.a $out/lib
    # NOTE: to build abcBridge, we need a copy of the source to use as an
    # include directory, so doing it here...
    cd ..
    mkdir -p $out/include
    cp -r src/* $out/include
    # This file is garbage on GitHub and causes issues
    rm $out/include/sat/glucose/stdint.h
  '';

  enableParallelBuilding = true;

  # buildPhase = ''
  #   make -j$NPROC ARCHFLAGS="-DABC_LIB -m64 -fPIC -DLIN64 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -DSIZEOF_INT=4" ABC_USE_NO_PTHREADS=1 ABC_USE_NO_READLINE=1 libabc.a abc
  # '';

  meta = {
    description = "A tool for sequential logic synthesis and formal verification";
    homepage    = "https://people.eecs.berkeley.edu/~alanmi/abc/abc.htm";
    license     = pkgs.lib.licenses.mit;
    platforms   = pkgs.lib.platforms.unix;
    maintainers = [ pkgs.lib.maintainers.ptival ];
  };

}
