{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.intellij;
in {
  options.mario.modules.editors.intellij = {
    enable = mkEnableOption "intellij";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.jetbrains; [idea-community];
  };
}
