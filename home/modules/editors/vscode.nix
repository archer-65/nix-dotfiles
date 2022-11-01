{ config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.editors.vscode;
in {
  options.user-modules.editors.vscode = {
    enable = mkEnableOption "vscode";
  };

  config = mkIf cfg.enable { programs.vscode = { enable = true; }; };
}
