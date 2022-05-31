_: { options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.hardware.bluetooth;
in {
  options.modules.hardware.bluetooth = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
}
