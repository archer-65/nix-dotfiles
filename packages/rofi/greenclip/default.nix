{
  stdenv,
  makeWrapper,
  pkgs,
  lib,
}: let
  name = "rofi-clipboard";
  script = (pkgs.writeScriptBin "${name}" (builtins.readFile ./greenclip.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
  pkgs.symlinkJoin {
    inherit name;
    paths = with pkgs; [rofi] ++ [script];
    nativeBuildInputs = [makeWrapper];
    postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    meta = {description = "My rofi greenclip (clipboard) launch script";};
  }
