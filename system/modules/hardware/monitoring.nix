{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.hardware.monitoring;
in {
  options.system.modules.hardware.monitoring = {
    enable = mkEnableOption "monitoring packages";

    corectrl.enable = mkEnableOption "corectrl and polkit config to avoid password";
  };

  config = mkIf cfg.enable (mkMerge [
    {environment.systemPackages = with pkgs; [lm_sensors smartmontools];}

    (mkIf cfg.corectrl.enable {
      programs.corectrl.enable = true;
      primaryUser.extraGroups = ["corectrl"];
    })
  ]);
}
