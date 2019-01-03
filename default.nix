{ pkgs ? import ./nixpkgs.nix }:
let
  inherit (pkgs.haskell.packages) ghcjs;

  drv = ghcjs.callCabal2nix "miso-jswidget-example" ./. {};

  final = pkgs.runCommand "miso-jswidget-example" {} ''
    mkdir $out
    cp ${drv}/bin/main.jsexe/all.js $out/all.js
    cp ${./html-src/index.html} $out/index.html
  '';
in
  if pkgs.lib.inNixShell then drv.env else final
