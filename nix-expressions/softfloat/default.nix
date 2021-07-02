{ cmake, fixDarwinDylibNames, pkgs, readline, source, stdenv }:
stdenv.mkDerivation rec {
  name = "berkeley-softfloat-3-${version}";
  src = source;
  version = source.rev;

  nativeBuildInputs = [ pkgs.gcc ]
    ++ pkgs.lib.optional stdenv.isDarwin fixDarwinDylibNames;

  # NOTE: this is specialized for Darwin, if you want to extend it to Linux,
  # check out:
  # https://github.com/GaloisInc/softfloat-hs/blob/master/Makefile
  CFLAGS = "-dynamiclib";
  LIB_NAME = "libsoftfloat1.dylib";
  SOFTFLOAT_PATH = "build/Linux-x86_64-GCC";
  SPECIALIZE_TYPE = "8086-SSE";

  buildPhase = ''
    (cd ${SOFTFLOAT_PATH} && make SPECIALIZE_TYPE=${SPECIALIZE_TYPE})
    gcc ${CFLAGS} -o ${LIB_NAME} ${SOFTFLOAT_PATH}/*.o
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp ${SOFTFLOAT_PATH}/softfloat.a $out/lib # static library
    cp ${LIB_NAME} $out/lib                   # dynamic library
    mkdir -p $out/include
    cp source/include/softfloat.h       $out/include
    cp source/include/softfloat_types.h $out/include
  '';

  enableParallelBuilding = true;

  meta = {
    description = "Software implementation of IEEE binary floating-point";
    homepage    = "https://github.com/ucb-bar/berkeley-softfloat-3/tree/master";
    license     = pkgs.lib.licenses.bsd3;
    platforms   = pkgs.lib.platforms.unix;
    maintainers = [ pkgs.lib.maintainers.ptival ];
  };

}
