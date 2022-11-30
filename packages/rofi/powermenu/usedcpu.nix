{
  stdenv,
  makeWrapper,
  pkgs,
  lib,
}: let
  name = "usedcpu";
  script = (pkgs.writeScriptBin "${name}" (builtins.readFile ./usedcpu.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
  pkgs.symlinkJoin {
    inherit name;
    paths = with pkgs; [procps bash] ++ [script];
    nativeBuildInputs = [makeWrapper];
    postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    meta = {description = "Used cpu utility script for rofi";};
  }
