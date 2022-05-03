{ config, lib, pkgs, ... }:

{  
  services = {
    dbus.enable = true;
  
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
