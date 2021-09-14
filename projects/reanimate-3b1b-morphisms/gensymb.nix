{ stdenv, fetchurl, texlive, unzip }:

stdenv.mkDerivation rec {
  version = "1.0";
  pname = "gensymb";
  name = "${pname}-${version}";
  tlType = "run";

  src = fetchurl {
    url = "https://mirrors.ctan.org/macros/latex/contrib/was.zip";
    sha256 = "0vz6s5kwbbkdl9y85bfnwjfspd7silfbxiwcc86dr06db1ryqvim";
  };

  buildInputs = [
    texlive.combined.scheme-basic
    unzip
  ];

  # unpackPhase = ''
  #   mkdir gensymb
  #   cd gensymb
  #   unzip $src
  # '';

  buildPhase = ''
    latex ./gensymb.ins
  '';

  installPhase = "
    mkdir -p $out/tex/latex/gensymb
    cp gensymb.sty $out/tex/latex/gensymb
  ";

}
