_:
{ options, config, lib, ... }:

with lib;
let cfg = config.user-modules.desktop.services.locker;
in {
  options.user-modules.desktop.services.locker = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.betterlockscreen = {
      enable = true;
      inactiveInterval = 10;
      arguments = [ "blur" ];
    };
  };
}
