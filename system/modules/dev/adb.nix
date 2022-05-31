{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.dev.adb;
in {
  options.modules.dev.adb = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.adb.enable = true;
    user.extraGroups = [ "adbusers" ];
  };
}
