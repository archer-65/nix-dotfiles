{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.xorg;
in {
  options.mario.modules.xorg = {
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

    mario.modules.xorg = {
      autorandr.enable = true;
      greenclip.enable = true;
      locker.enable = true;
      picom.enable = true;
    };
  };
}
