{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.monitoring;
in {
  options.modules.hardware.monitoring = {
    enable = mkBoolOpt false;
    corectrl.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    { environment.systemPackages = with pkgs; [ lm_sensors smartmontools ]; }

    (mkIf cfg.corectrl.enable { programs.corectrl.enable = true; })
  ]);
}
