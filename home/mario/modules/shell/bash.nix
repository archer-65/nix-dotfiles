{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.bash;
in {
  options.mario.modules.shell.bash = {
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

    home.packages = [pkgs.beautysh];
  };
}
