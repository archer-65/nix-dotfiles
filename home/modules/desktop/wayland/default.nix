{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.desktop.wayland;
in {
  options.home.modules.desktop.wayland = {
    enable = mkEnableOption "wayland configuration management for user";

    wm = mkOption {
      description = "An option to choose the window manager [wayland] configuration to enable";
      default = null;
      type = types.nullOr (types.enum ["sway" "hyprland"]);
      example = "sway";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      wl-clipboard
      wtype
      # azote
      grim
      slurp
      sway-contrib.grimshot
    ];
  };
}
