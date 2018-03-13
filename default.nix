{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  # Running off fork for now
  miso-src = pkgs.fetchFromGitHub {
    rev = "5f1a0333656c092f36f4758bcf951bd179f68c3c";
    sha256 = "1ijm1i5bgmbalg0fq0wpcy4zdqvwpg3dpv81rwbr1r5s634hpj80";
    owner = "dmjio";
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
