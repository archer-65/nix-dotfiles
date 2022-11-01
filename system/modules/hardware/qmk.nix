{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.hardware.qmk;
in {
  options.system.modules.hardware.qmk = {
    enable = mkEnableOption "qmk and vial support";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [qmk vial];
    services.udev.packages = [pkgs.qmk-udev-rules];
  };
}
