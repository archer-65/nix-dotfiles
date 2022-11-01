{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.hardware.razer;
in {
  options.modules.hardware.razer = {
    enable = mkEnableOption "razer devices support";
  };

  config = mkIf cfg.enable {
    hardware.openrazer.enable = true;

    user.extraGroups = ["openrazer"];

    environment.systemPackages = with pkgs; [polychromatic];
  };
}
