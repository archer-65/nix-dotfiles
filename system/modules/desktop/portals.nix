{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfgXorg = config.system.modules.desktop.xorg;
  cfgWayland = config.system.modules.desktop.wayland;
in {
  # XDG Portals, useful for wayland screen sharing and flatpak).
  config = mkMerge [
    (mkIf (cfgXorg.enable || cfgWayland.enable) {
      xdg.portal = {
        enable = true;
        extraPortals = [pkgs.xdg-desktop-portal-gtk];
        # gtkUsePortal = true;
      };
    })

    (mkIf cfgWayland.enable {
      xdg.portal = {
        # wlr.enable = true;
        extraPortals = [pkgs.xdg-desktop-portal-wlr];
      };
    })
  ];
}
