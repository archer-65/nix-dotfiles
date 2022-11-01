{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.wayland;
  cfgTheme = config.user-modules.themes;
  inherit (config.dotfiles) configDir;

in {

  config = mkIf (cfg.enable && cfg.wm == "hyprland") {

    wayland.windowManager.hyprland = {
      enable = true;
      xwayland = {
        enable = true;
        hidpi = true;
      };
      systemdIntegration = true;

    };

    home.sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "sway";
      SDL_VIDEODRIVER = "wayland";
      GTK_USE_PORTAL = "1";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      MOZ_ENABLE_WAYLAND = "1";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      NIXOS_OZONE_WL = "1";
    };

    user-modules.desktop = {
      services = {
        dunst.enable = true;
        locker-wayland.enable = true;
        waybar.enable = true;
      };
    };
  };
}
