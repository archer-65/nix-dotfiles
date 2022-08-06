_:
{ config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.editors.vscode;
in {
  options.user-modules.editors.vscode = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable { programs.vscode = { enable = true; }; };
}
