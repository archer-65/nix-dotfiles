{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.desktop.xorg;
in {
  options.system.modules.desktop.xorg = {
    enable = mkEnableOption "xorg basic configuration and packages";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;
    # Disable `lightdm` because it is enabled by default sometimes (e.g. greetd with also `xserver` option enabled).
    services.xserver.displayManager.lightdm.enable = mkForce false;

    services.xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "ctrl:nocaps";
    };

    services.xserver.libinput.enable = true;

    services.xserver.windowManager = {qtile.enable = true;};
  };
}
