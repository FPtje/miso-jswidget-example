{ mkDerivation, base, lens, miso, stdenv }:
mkDerivation {
  pname = "miso-jswidget-example";
  version = "0.1.0.0";
  src = stdenv.lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base lens miso ];
  description = "Embedding a JS widget in Miso example";
  license = stdenv.lib.licenses.unfree;
}
