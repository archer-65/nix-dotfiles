_: { options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.dev.adb;
in {
  options.modules.dev.adb = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.adb.enable = true;
    user.extraGroups = [ "adbusers" ];
  };
}
