{ stdenv, makeWrapper, pkgs, lib }:
let name = "usedcpu";
in stdenv.mkDerivation {
  name = "${name}";
  src = ../../bin;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = with pkgs; ''
        mkdir -p $out/bin
        install -Dm 755 ./usedcpu.sh $out/bin/${name}
    	  wrapProgram $out/bin/${name} --prefix PATH ":" ${
         lib.makeBinPath [ bash procps ]
       }
  '';

  meta = { description = "Used cpu utility script for rofi"; };
}
