{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.desktop.wayland;
in {
  options.system.modules.desktop.wayland = {
    enable = mkEnableOption "wayland basic configuration and packages";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [sway wayland glib];

    programs.sway = {
      enable = true;

      extraSessionCommands = ''
        export XDG_SESSION_TYPE=wayland
        export XDG_CURRENT_DESKTOP=sway
        export SDL_VIDEODRIVER=wayland
        export GTK_USE_PORTAL=1
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export MOZ_ENABLE_WAYLAND=1
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';

      wrapperFeatures = {
        base = true;
        gtk = true;
      };
    };
  };
}
