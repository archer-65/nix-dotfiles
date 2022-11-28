{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.xorg;
  inherit (config.dotfiles) configDir;
in {
  config = mkIf (cfg.enable && cfg.wm == "qtile") {
    xsession.enable = true;

    xdg.configFile."qtile" = {
      source = "${configDir}/qtile";
      recursive = true;
    };

    services = {pasystray.enable = true;};

    mario.modules.desktop = {
      apps = {autorandr.enable = true;};

      services = {
        picom.enable = true;
        dunst.enable = true;
      };
    };
  };
}
