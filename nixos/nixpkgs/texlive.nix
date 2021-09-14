{ pkgs ? import <nixpkgs> {}
}:
with pkgs;
texlive.combine {
  inherit (texlive)

  #scheme-minimal # plain
  #scheme-basic   # + latex
  #scheme-small   # + xetex
  scheme-medium  # + packages
  #scheme-full    # + more packages

  adjustbox
  algorithm2e
  anyfontsize
  babel
  babel-greek
  biblatex
  bigfoot         # needed by cprotect for suffix
  blindtext
  booktabs
  boondox
  bussproofs
  caption
  cbfonts
  ccicons
  cleveref
  cmap
  collectbox
  collection-fontsrecommended
  collection-pictures
  comment
  cprotect
  currvita
  dejavu
  doublestroke
  draftwatermark
  enumitem
  environ
  etoolbox
  euenc
  everypage
  filehook
  float
  fontaxes
  fontawesome
  fontspec
  framed         # needed by minted
  fvextra        # needed by minted
  gfsartemisia
  gfsbaskerville
  gfsdidot
  gfsneohellenic
  greek-fontenc
  greektex
  ifplatform     # needed by minted
  inconsolata
  inlinedef      # needed by forest
  latexmk
  libertine
  listings
  logreq         # needed by biblatex
  ltablex
  makecell
  mathpartir
  mdframed
  mdwtools
  metafont
  microtype
  minted
  ms
  mweights
  ncctools
  needspace      # needed by mdframed
  newtx
  relsize
  sfmath
  soul
  standalone
  stmaryrd
  tcolorbox
  textcase
  titlesec
  tocloft
  totpages
  trimspaces
  ucs
  upquote
  xcolor
  xetex
  #xetex-def
  xpatch
  xstring
  ;
}
