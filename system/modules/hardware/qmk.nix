{ config, options, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hardware.qmk;
in {
  options.modules.hardware.qmk = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ qmk vial ];
    services.udev.packages = [ pkgs.qmk-udev-rules ];
  };
}
