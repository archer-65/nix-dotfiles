{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfgXorg = config.system.modules.graphical.xorg;
  cfgWayland = config.system.modules.graphical.wayland;
in {
  # XDG Portals, useful for wayland screen sharing and flatpak).
  config = mkMerge [
    (mkIf (cfgXorg.enable || cfgWayland.enable) {
      xdg.portal.enable = true;
    })

    (mkIf cfgWayland.enable {
      xdg.portal = {
        extraPortals = lib.mkForce [pkgs.xdg-desktop-portal-gtk];
        wlr = {
          enable = true;
          settings.screencast = {
            chooser_type = "simple";
            chooser_cmd = "${pkgs.slurp}/bin/slurp -f %o -or";
          };
        };
      };
    })
  ];
}
