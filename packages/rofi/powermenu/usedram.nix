{
  stdenv,
  makeWrapper,
  pkgs,
  lib,
}: let
  name = "usedram";
  script = (pkgs.writeScriptBin "${name}" (builtins.readFile ./usedram.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
  pkgs.symlinkJoin {
    inherit name;
    paths = with pkgs; [procps bash] ++ [script];
    nativeBuildInputs = [makeWrapper];
    postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    meta = {description = "Used ram utility script for rofi";};
  }
