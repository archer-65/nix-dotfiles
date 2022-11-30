{
  stdenv,
  makeWrapper,
  pkgs,
  lib,
}: let
  name = "rofi-powermenu";
  script = (pkgs.writeScriptBin "${name}" (builtins.readFile ./powermenu.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
  pkgs.symlinkJoin {
    inherit name;
    paths = with pkgs; [procps bash rofi] ++ [script];
    nativeBuildInputs = [makeWrapper];
    postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    meta = {description = "My rofi powermenu launch script";};
  }
