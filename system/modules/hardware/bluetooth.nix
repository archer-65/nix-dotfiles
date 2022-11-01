{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.hardware.bluetooth;
in {
  options.modules.hardware.bluetooth = {
    enable = mkEnableOption "bluetooth and blueman service";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth.enable = true;

    services = {blueman.enable = true;};
  };
}
