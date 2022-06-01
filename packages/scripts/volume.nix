{ stdenv, makeWrapper, pkgs, lib }:
let name = "volume";
in stdenv.mkDerivation {
  name = "${name}";
  src = ../../bin;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = with pkgs; ''
    mkdir -p $out/bin
    install -Dm 755 ./volume.sh $out/bin/${name}
    wrapProgram $out/bin/${name} --prefix PATH ":" ${
      lib.makeBinPath [ bash dunst pamixer ]
    }
  '';

  meta = { description = "Volume adjusting script"; };
}
