_:
{ config, ... }:

let configDir = config.dotfiles.configDir;
in {
  xsession.enable = true;

  xdg.configFile."qtile" = {
    source = "${configDir}/qtile";
    recursive = true;
  };

  programs = {
    autorandr.enable = true; 
  };
  
  services = {
    pasystray.enable = true;
  };

  user-modules.desktop = {
    services = {
      picom.enable = true;
      dunst.enable = true;
    };
  };
}
