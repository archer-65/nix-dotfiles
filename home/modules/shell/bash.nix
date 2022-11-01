{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.shell.bash;
in {
  options.home.modules.shell.bash = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;
      shellAliases = {};
    };
  };
}
