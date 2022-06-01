{ stdenv, makeWrapper, pkgs, lib }:
let name = "rofi_emoji";
in stdenv.mkDerivation {
  name = "${name}";
  src = ../bin;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = with pkgs; ''
        mkdir -p $out/bin
        install -Dm 755 ./emoji.sh $out/bin/${name}
    	  wrapProgram $out/bin/${name} --prefix PATH ":" ${
         lib.makeBinPath [ rofi bash ]
       }
  '';

  meta = { description = "My rofi emoji launch script"; };
}
