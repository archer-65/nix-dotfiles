{ stdenv, makeWrapper, pkgs, lib }:
let name = "usedram";
in stdenv.mkDerivation {
  name = "${name}";
  src = ../bin;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = with pkgs; ''
        mkdir -p $out/bin
        install -Dm 755 ./usedram.sh $out/bin/${name}
    		wrapProgram $out/bin/${name} --prefix PATH ":" ${lib.makeBinPath [ bash ]}
  '';

  meta = { description = "Used ram utility script for rofi"; };
}
