{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  # Running off fork for now
  miso-src = pkgs.fetchFromGitHub {
    rev = "1dc5b88c16a9ca6a2c22e738bea7c96e56acdacb";
    sha256 = "0imasiyp81xnp92rxdfpprz3m8la79lsacvchglclvagfwaahqdd";
    owner = "FPtje";
    repo = "miso";
  };

  miso-ghcjs = ghcjsHEAD.callCabal2nix "miso" miso-src {};

  drv = ghcjsHEAD.callPackage ./pkg.nix { miso = miso-ghcjs; };

  final = pkgs.runCommand "miso-jswidget-example" {} ''
    mkdir $out
    cp ${drv}/bin/main.jsexe/all.js $out/all.js
    cp ${./html-src/index.html} $out/index.html
  '';
in
  if pkgs.lib.inNixShell then drv.env else final
