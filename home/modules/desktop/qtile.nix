_: { pkgs, config, ... }:

let
  configDir = config.dotfiles.configDir; 
in
{
  xsession.enable = true;

  xdg.configFile."qtile" = {
    # source = ../../../config/qtile;
    source = "${configDir}/qtile";
    recursive = true;
  };

  programs.autorandr = { enable = true; };

  user-modules.desktop = {
    apps.rofi.enable = true;

    services = {
      picom.enable = true;
      dunst.enable = true;
    };
  };
}
