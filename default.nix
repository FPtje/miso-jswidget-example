{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  # Running off fork for now
  miso-src = pkgs.fetchFromGitHub {
    rev = "f92ab2c5eb067dfd7b37e0f7c7ee565cdf3c04dd";
    sha256 = "0vih1gya3f94jpgp8jx2l3lxgpgqc8qqbf9qbnr8kjzjw994kcx2";
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
