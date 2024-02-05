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
  inherit (config.colorScheme) palette;
in {
  options.mario.modules.wayland.locker = {
    enable = mkEnableOption "wayland screen locker";
  };

  config = mkIf cfg.enable {
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

        layout-text-color = palette.base0E;

        bs-hl-color = palette.base08;
        key-hl-color = palette.base0B;
        separator-color = palette.base05;

        text-color = palette.base07;
        text-clear-color = palette.base07;
        text-ver-color = palette.base07;
        text-wrong-color = palette.base07;

        inside-color = transparent;
        inside-clear-color = transparent;
        inside-ver-color = transparent;
        inside-wrong-color = transparent;

        line-color = transparent;
        line-clear-color = transparent;
        line-ver-color = transparent;
        line-wrong-color = transparent;

        ring-color = palette.base01;
        ring-clear-color = palette.base0A;
        ring-ver-color = palette.base0E;
        ring-wrong-color = palette.base08;
      };
    };

    services.swayidle = let
      swaylock = "${pkgs.swaylock-effects}/bin/swaylock -fF";
    in {
      enable = true;

      events = [
        {
          event = "before-sleep";
          command = "${swaylock}";
        }
        {
          event = "lock";
          command = "${swaylock}";
        }
      ];

      timeouts = let
        hyprctl = "${pkgs.hyprland}/bin/hyprctl";
        swaymsg = "${pkgs.sway}/bin/swaymsg";
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
