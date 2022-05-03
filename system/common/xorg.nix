{ config, lib, pkgs, ... }:

{
  services = {
    dbus.enable = true;
    
    pkgs.overlays = [
      (self: super: {
        qtile = super.qtile.overrideAttrs (oldAttrs: {
          pythonPath = oldAttrs.pythonPath ++ (with self.python37Packages; [
            keyring
            xcffib
            #cairocffi-xcffib
            setuptools
            setuptools_scm
            dateutil
            dbus-python
            mpd2
            psutil
            pyxdg
            pygobject3
          ]);
        });
      })
    ];

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
