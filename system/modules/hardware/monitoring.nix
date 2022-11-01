{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.hardware.monitoring;
in {
  options.modules.hardware.monitoring = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };

    corectrl.enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    { environment.systemPackages = with pkgs; [ lm_sensors smartmontools ]; }

    (mkIf cfg.corectrl.enable {
      programs.corectrl.enable = true;

      security.polkit.extraConfig = ''
        polkit.addRule(function(action, subject) {
          if ((action.id == "org.corectrl.helper.init" ||
              action.id == "org.corectrl.helperkiller.init") &&
              subject.local == true &&
              subject.active == true &&
              subject.isInGroup("users")) {
                  return polkit.Result.YES;
          }
        });
      '';
    })
  ]);
}
