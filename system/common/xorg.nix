{ config, lib, pkgs, ... }:

{
  programs.dconf.enable = true;
     
  services = {
    gnome.gnome-keyring.enable = true;

    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    xserver = {
      enable = true;

      layout = "us";
      xkbVariant = "intl";

      libinput = {
        enable = true;
      };

      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+qtile";
      };

      desktopManager.xfce.enable = true;

      windowManager.qtile = {
        enable = true;
      };
    };
  };
}
