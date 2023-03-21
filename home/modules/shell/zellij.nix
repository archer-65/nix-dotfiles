{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.zellij;
in {
  options.mario.modules.shell.zellij = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.zellij = {
      enable = true;
      settings = {};
    };
  };
}
