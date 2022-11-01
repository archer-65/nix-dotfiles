{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.shell.bash;
in {
  options.user-modules.shell.bash = {
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
