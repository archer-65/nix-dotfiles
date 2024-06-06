{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.xorg;
in {
  options.system.modules.graphical.xorg = {
    enable = mkEnableOption "xorg basic configuration and packages";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;

    services.xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "ctrl:nocaps";
    };

    services.libinput.enable = true;

    services.xserver.windowManager = {qtile.enable = true;};
  };
}
