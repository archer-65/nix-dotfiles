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
        defaultSession = "none+qtile";
      };

      windowManager.qtile = {
        enable = true;
      };
    };
  };
}
