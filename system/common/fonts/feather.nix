{ stdenv, lib }:

stdenv.mkDerivation {
  name = "Feather";
  src = ./Feather.ttf;

  phases = ["installPhase"];

  installPhase = ''
    install -D $src $out/share/fonts/truetype/Feather.ttf
  '';
}