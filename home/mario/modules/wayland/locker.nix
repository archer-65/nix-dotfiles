{
  options,
  config,
  lib,
  pkgs,
  outputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.wayland.locker;
  cfgSway = config.wayland.windowManager.sway;
  cfgHyprland = config.wayland.windowManager.hyprland;

  cfgTheme = config.mario.modules.themes;
in {
  options.mario.modules.wayland.locker = {
    enable = mkEnableOption "wayland screen locker";
  };

  config = mkIf cfg.enable {
    stylix.targets.swaylock.enable = true;

    programs.swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;
      settings = let
        transparent = "#00000000";
      in {
        font = cfgTheme.font.regular.family;
        image = outputs.wallpapers.digital-flowers.src;
        effect-blur = "5x5";

        ignore-empty-password = true;
        hide-keyboard-layout = true;

        clock = true;
        indicator = true;
        indicator-radius = 80;
        indicator-thickness = 15;
      };
    };

    services.swayidle = let
      swaylock = "${pkgs.swaylock-effects}/bin/swaylock -fF";
    in {
      enable = true;

      events = {
        before-sleep = "${swaylock}";
        lock = "${swaylock}";
      };

      timeouts = let
        hyprctl = "${config.wayland.windowManager.hyprland.finalPackage}/bin/hyprctl";
        swaymsg = "${config.wayland.windowManager.sway.package}/bin/swaymsg";
      in
        [
          {
            timeout = 300;
            command = "${swaylock}";
          }
        ]
        ++ (optionals cfgSway.enable
          [
            {
              timeout = 360;
              command = "${swaymsg} output * dpms off";
              resumeCommand = "${swaymsg} output * dpms on";
            }
          ])
        ++ (optionals cfgHyprland.enable [
          {
            timeout = 360;
            command = "${hyprctl} dispatch dpms off";
            resumeCommand = "${hyprctl} dispatch dpms on";
          }
        ]);
    };

    systemd.user.services.swayidle.Install = {
      WantedBy =
        (optionals cfgSway.enable [
          "sway-session.target"
        ])
        ++ (optionals cfgHyprland.enable [
          "hyprland-session.target"
        ]);
    };
  };
}
