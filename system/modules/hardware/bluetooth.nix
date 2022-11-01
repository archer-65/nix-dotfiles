{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.hardware.bluetooth;
in {
  options.system.modules.hardware.bluetooth = {
    enable = mkEnableOption "bluetooth and blueman service";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth.enable = true;

    services = {blueman.enable = true;};
  };
}
