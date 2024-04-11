{
  lib,
  pkgs,
  stdenv,
  makeWrapper,
  procps,
  # wayland-only deps
  rofi-wayland,
  # x11-only deps
  rofi,
  # backend selector
  backend ? "x11",
}:
with lib; let
  name = "rofi-powermenu";
in
  assert lib.assertOneOf "backend" backend ["x11" "wayland"];
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
          makeBinPath ([
              procps
            ]
            ++ lib.optionals (backend == "x11") [
              rofi
            ]
            ++ lib.optionals (backend == "wayland") [
              rofi-wayland
            ])
        }"
      '';

      meta = {
        description = "A rofi graphical powermenu script";
        platforms = platforms.all;
        mainProgram = "${name}";
      };
    }
