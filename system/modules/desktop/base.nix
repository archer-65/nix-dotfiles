_:
{ lib, config, pkgs, ... }:

with lib;

let cfgDependency = config.modules.desktop.xorg;
in {
  config = mkIf cfgDependency.enable {
    services.gnome.gnome-keyring.enable = true;

    programs.dconf.enable = true;
    services.dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    programs.qt5ct.enable = true;
  };
}
