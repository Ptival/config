{ fetchFromGitHub, lib, nenofex, picosat, stdenv }:

let
  version = "6.03";
in

stdenv.mkDerivation {
  name = "depqbf-${version}";

  src = fetchFromGitHub {
    owner = "lonsing";
    repo = "depqbf";
    rev = "2ad3995a21b8ca2831be2534b1cb80df342b3bdf";
    sha256 = "sha256-qo7K6jkdIfYeIYv3g3lohFC1R023/9+J8KAwLpWsr2Q=";
  };

  buildInputs = [
    nenofex
    picosat
  ];
  propagatedBuildInputs = [
  ];

  # installPhase = ''
  #   make -f Makefile CoqMakefile
  #   make -f CoqMakefile COQLIB=$out/lib/coq/${coq.coq-version}/ install
  # '';

  meta = with lib; {
    homepage = https://github.com/lonsing/depqbf;
    description = "TODO";
    license = licenses.asl20;
    maintainers = with maintainers; [ ptival ];
    # platforms = coq.meta.platforms;
  };

}
