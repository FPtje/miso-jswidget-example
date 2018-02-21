{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  # Running off fork for now
  miso-src = pkgs.fetchFromGitHub {
    rev = "c90762f970e4a9467cdfd741573a2cf8aa6b12ab";
    sha256 = "1jqx67a7j1g4zwpfyzaxh393s38c0qx9rbfnmmzw975rx1f4jwgk";
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
