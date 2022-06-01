{ stdenv, makeWrapper, pkgs, lib }:
let name = "rofi_powermenu";
in stdenv.mkDerivation {
  name = "${name}";
  src = ../bin;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = with pkgs; ''
        mkdir -p $out/bin
        install -Dm 755 ./powermenu.sh $out/bin/${name}
    	  wrapProgram $out/bin/${name} --prefix PATH ":" ${
         lib.makeBinPath [ bash rofi procps ]
       }
  '';

  meta = { description = "My rofi powermenu launch script"; };
}
