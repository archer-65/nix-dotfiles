{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.xorg.locker;
in {
  options.mario.modules.xorg.locker = {
    enable = mkEnableOption "xorg screen locker";
  };

  config = mkIf cfg.enable {
    services.screen-locker = {xss-lock.extraOptions = ["-l"];};

    services.betterlockscreen = {
      enable = true;
      inactiveInterval = 10;
      arguments = ["blur"];
    };
  };
}
