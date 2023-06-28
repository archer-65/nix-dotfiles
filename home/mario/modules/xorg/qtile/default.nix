{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.xorg;
in {
  config = mkIf (cfg.enable && cfg.wm == "qtile") {
    xsession.enable = true;

    xdg.configFile."qtile" = {
      source = ./config;
      recursive = true;
    };

    services = {pasystray.enable = true;};

    mario.modules = {
      apps.dunst.enable = true;
    };
  };
}
