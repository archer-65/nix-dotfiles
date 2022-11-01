{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.editors.intellij;
in {
  options.home.modules.editors.intellij = {
    enable = mkEnableOption "intellij";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.jetbrains; [idea-community];
  };
}
