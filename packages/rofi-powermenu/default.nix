{
  lib,
  pkgs,
  stdenv,
  makeWrapper,
  procps,
  rofi,
}:
with lib; let
  name = "rofi-powermenu";
in
  stdenv.mkDerivation {
    inherit name;
    version = "1.0";
    src = ./powermenu.sh;

    nativeBuildInputs = [makeWrapper];

    dontUnpack = true;
    dontBuild = true;
    dontConfigure = true;

    installPhase = ''
      install -Dm 0755 $src $out/bin/${name}
      wrapProgram $out/bin/${name} --prefix PATH ':' \
        "${
        makeBinPath [
          procps
          rofi
        ]
      }"
    '';

    meta = {
      description = "A rofi graphical powermenu script";
      platforms = platforms.all;
      mainProgram = "${name}";
    };
  }
