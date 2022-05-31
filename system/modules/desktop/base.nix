{ lib, config, pkgs, ... }:

with lib;
with lib.my;
{
  config = mkIf config.services.xserver.enable {
    services.gnome.gnome-keyring.enable = true;
     
    programs.dconf.enable = true;
    services.dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
  };
}