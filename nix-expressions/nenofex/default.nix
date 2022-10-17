{ clang, fetchFromGitHub, lib, picosat, stdenv }:

let
  version = "1.1";
in

stdenv.mkDerivation {
  name = "nenofex-${version}";

  src = fetchFromGitHub {
    owner = "lonsing";
    repo = "nenofex";
    rev = "1009d00b91a0476869bdc549f3478b7bdf7baca1";
    sha256 = "sha256-5LJGJ8LTHRFXqUW6vW1kZYCh2U4HwHDVsIp67FRaLnU=";
  };

  patches = [
    ./nenofex.patch
  ];

  buildInputs = [
    picosat
  ];
  propagatedBuildInputs = [
  ];

  # installPhase = ''
  #   make -f Makefile CoqMakefile
  #   make -f CoqMakefile COQLIB=$out/lib/coq/${coq.coq-version}/ install
  # '';

  meta = with lib; {
    homepage = https://github.com/lonsing/nenofex;
    description = "TODO";
    license = licenses.asl20;
    maintainers = with maintainers; [ ptival ];
    # platforms = coq.meta.platforms;
  };

}
