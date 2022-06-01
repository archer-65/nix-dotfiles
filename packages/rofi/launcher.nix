{ stdenv, makeWrapper, pkgs, lib }:
let name = "rofi_launcher";
in stdenv.mkDerivation {
  name = "${name}";
  src = ../../bin;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = with pkgs; ''
        mkdir -p $out/bin
        install -Dm 755 ./launcher.sh $out/bin/${name}
    	  wrapProgram $out/bin/${name} --prefix PATH ":" ${
         lib.makeBinPath [ bash rofi ]
       }
  '';

  meta = { description = "My rofi launcher launch script"; };
}
