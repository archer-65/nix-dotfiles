_: { lib, config, pkgs, ... }:

with lib;
let 
  cfgDependency = config.modules.desktop.xorg;
in
{
  # XDG Portals, useful for wayland screen sharing and flatpak).
  config = mkIf cfgDependency.enable {
    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
  };
}
