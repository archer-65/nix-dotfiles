{
  makeWrapper,
  pkgs,
}: let
  name = "theme-toggle";
  script = (pkgs.writeScriptBin "${name}" (builtins.readFile ./theme-toggle.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
  pkgs.symlinkJoin {
    inherit name;
    paths = [script];
    nativeBuildInputs = [makeWrapper];
    postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
  }
