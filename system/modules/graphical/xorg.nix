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

      displayManager.lightdm.enable = true;
      displayManager.defaultSession = "none+qtile";

      windowManager.qtile.enable = true;
    };
  };
}
