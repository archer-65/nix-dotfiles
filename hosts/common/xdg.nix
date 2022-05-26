{ lib, config, pkgs, ... }:

{
  # XDG Portals, useful for wayland screen sharing and flatpak).
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };
}