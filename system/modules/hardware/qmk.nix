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
    hardware.keyboard.qmk.enable = true;

    environment.systemPackages = with pkgs; [qmk vial];

    primaryUser.extraGroups = ["plugdev"];
  };
}
