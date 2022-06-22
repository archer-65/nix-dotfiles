_:
{ config, options, lib, ... }:

with lib;
let
  cfg = config.user-modules.desktop.xorg.qtile;
  cfgXorg = config.user-modules.desktop.xorg;
  configDir = config.dotfiles.configDir;
in {
  options.user-modules.desktop.xorg.qtile = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf (cfgXorg.enable && cfg.enable) {

    xsession.enable = true;

    xdg.configFile."qtile" = {
      source = "${configDir}/qtile";
      recursive = true;
    };

    services = {
      pasystray.enable = true;
    };

    user-modules.desktop = {
      apps = {
        autorandr.enable = true;
      };

      services = {
        picom.enable = true;
        dunst.enable = true;
      };
    };
  };
}
