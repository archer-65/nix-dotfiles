{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.wayland;
in {
  options.mario.modules.wayland = {
    enable = mkEnableOption "wayland configuration management for user";
    wm = mkOption {
      description = "An option to choose the window manager [wayland] configuration to enable";
      default = [];
      type = with types; listOf (enum ["sway" "hyprland" ""]);
      example = ["sway"];
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      wl-clipboard
      wtype
      grim
      slurp
      sway-contrib.grimshot
      swaybg
    ];

    services.cliphist = {
      enable = true;
      allowImages = true;
    };

    services.wl-clip-persist = {
      enable = true;
      clipboardType = "regular";
    };

    home.sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      SDL_VIDEODRIVER = "wayland";
      QT_QPA_PLATFORM = "wayland;xcb";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      NIXOS_OZONE_WL = "1";
    };
  };
}
