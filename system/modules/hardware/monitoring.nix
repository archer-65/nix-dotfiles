{ options, config, lib, pkgs, ... }:

with lib;
;
let cfg = config.modules.hardware.monitoring;
in {
  options.modules.hardware.monitoring = {
    enable = _.mkBoolOpt false;
    corectrl.enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    { environment.systemPackages = with pkgs; [ lm_sensors smartmontools ]; }

    (mkIf cfg.corectrl.enable { programs.corectrl.enable = true; })
  ]);
}
