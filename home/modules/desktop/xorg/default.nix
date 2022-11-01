{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.desktop.xorg;
in {
  options.home.modules.desktop.xorg = {
    enable = mkEnableOption "xorg configuration management for user";

    wm = mkOption {
      description = "An option to choose the window manager [xorg] configuration to enable";
      default = null;
      type = types.nullOr (types.enum ["qtile"]);
      example = "qtile";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [nitrogen xclip xdotool];

    services = {flameshot.enable = true;};

    home.modules.desktop = {
      apps = {greenclip.enable = true;};
      services = {locker.enable = true;};
    };
  };
}
