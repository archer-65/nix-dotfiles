{
  makeWrapper,
  pkgs,
  lib,
}: let
  name = "volume";
  script = (pkgs.writeScriptBin "${name}" (builtins.readFile ./volume.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
  pkgs.symlinkJoin {
    inherit name;
    paths = with pkgs; [dunst pamixer] ++ [script];
    nativeBuildInputs = [makeWrapper];
    postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    meta = {description = "Volume adjusting script";};
  }
