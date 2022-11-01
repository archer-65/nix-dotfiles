{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.editors.vscode;
in {
  options.home.modules.editors.vscode = {
    enable = mkEnableOption "vscode";
  };

  config = mkIf cfg.enable {programs.vscode = {enable = true;};};
}
