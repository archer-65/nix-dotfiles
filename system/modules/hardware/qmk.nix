{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.hardware.qmk;
in {
  options.modules.hardware.qmk = {
    enable = mkEnableOption "qmk and vial support";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [qmk vial];
    services.udev.packages = [pkgs.qmk-udev-rules];
  };
}
