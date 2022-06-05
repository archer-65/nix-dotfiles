_:
{ config, ... }:

let configDir = config.dotfiles.configDir;
in {
  xsession.enable = true;

  xdg.configFile."qtile" = {
    source = "${configDir}/qtile";
    recursive = true;
  };

  services = { pasystray.enable = true; };

  user-modules.desktop = {
    apps = { autorandr.enable = true; };

    services = {
      picom.enable = true;
      dunst.enable = true;
    };
  };
}
