{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.vscode;
in {
  options.mario.modules.editors.vscode = {
    enable = mkEnableOption "vscode";
  };

  config = mkIf cfg.enable {programs.vscode = {enable = true;};};
}
