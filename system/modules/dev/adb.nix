{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.adb;
in {
  options.system.modules.dev.adb = {
    enable = mkEnableOption "adb configuration";
  };

  config = mkIf cfg.enable {
    programs.adb.enable = true;
    user.extraGroups = ["adbusers"];
  };
}
