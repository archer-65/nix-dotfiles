{ lib, config, pkgs, ... }:

with lib;
with lib.my;
{
  # XDG Portals, useful for wayland screen sharing and flatpak).
  config = mkIf config.services.xserver.enable {
    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
  };
}