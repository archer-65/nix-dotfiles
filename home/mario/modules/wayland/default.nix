{
  config,
  options,
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
      wl-clip-persist
      wtype
      grim
      slurp
      sway-contrib.grimshot
      swaybg
      clipman
    ];

    home.sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      XDG_SESSION_TYPE = "wayland";
      SDL_VIDEODRIVER = "wayland";
      # GTK_USE_PORTAL = "1";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      MOZ_ENABLE_WAYLAND = "1";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      NIXOS_OZONE_WL = "1";
      # WLR_RENDERER_ALLOW_SOFTWARE = "1";
    };
  };
}
