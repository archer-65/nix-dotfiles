{ pkgs, config, ... }:

{
  xsession.enable = true;

  xdg.configFile."qtile" = {
    source = ../../../config/qtile;
    recursive = true;
  };

  programs.autorandr = { enable = true; };

  user-modules.desktop = {
    apps.rofi = true;

    services = {
      picom.enable = true;
      dunst.enable = true;
    };
  };
}
