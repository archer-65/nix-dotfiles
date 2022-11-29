{
  stdenv,
  makeWrapper,
  pkgs,
  lib,
}: let
  name = "theme-toggle";
in
  stdenv.mkDerivation {
    name = "${name}";
    src = ../../bin;
    nativeBuildInputs = [makeWrapper];
    installPhase = with pkgs; ''
        mkdir -p $out/bin
        install -Dm 755 ./${name}.sh $out/bin/${name}
      wrapProgram $out/bin/${name} --prefix PATH ":" ${lib.makeBinPath [bash]}
    '';

    meta = {description = "Toggle for dark and light theme";};
  }
