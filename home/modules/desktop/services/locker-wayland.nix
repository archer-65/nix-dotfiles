{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.services.locker-wayland;
in {
  options.mario.modules.desktop.services.locker-wayland = {
    enable = mkEnableOption "wayland screen locker";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [swaylock-effects];

    xdg.configFile."swaylock/config".text = let
      transparent = "#00000000";
    in ''
      clock
      effect-blur=5x5
      indicator-radius=80
      hide-keyboard-layout
      image=~/pics/walls/weebie/wallhaven-j3mmdy.jpg
      ring-color=#D1D5DB
      ring-clear-color=#D1D5DB
      ring-ver-color=#D1D5DB
      ring-wrong-color=#FCA5A5
      key-hl-color=#D1D5DB
      bs-hl-color=#FCA5A5
      text-color=${transparent}
      text-clear-color=${transparent}
      text-caps-lock-color=${transparent}
      text-ver-color=${transparent}
      text-wrong-color=${transparent}
      line-color=${transparent}
      line-clear-color=${transparent}
      line-caps-lock-color=${transparent}
      line-ver-color=${transparent}
      line-wrong-color=${transparent}
      inside-color=${transparent}
      inside-clear-color=${transparent}
      inside-ver-color=${transparent}
      inside-wrong-color=${transparent}
      separator-color=${transparent}
    '';

    services.swayidle = let
      swaylock = "${pkgs.swaylock-effects}/bin/swaylock -fF";
      hyprctl = "${pkgs.hyprland}/bin/hyprctl";
      swaymsg = "${pkgs.sway}/bin/swaymsg";
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

      timeouts = [
        {
          timeout = 360;
          command = "${swaymsg} 'output * dpms off'";
          resumeCommand = "${swaymsg} 'output * dpms on'";
        }
        {
          timeout = 360;
          command = "${hyprctl} 'dispatch dpms off'";
          resumeCommand = "${hyprctl} 'dispatch dpms on'";
        }
        {
          timeout = 300;
          command = "${swaylock}";
        }
      ];

      systemdTarget = "sway-session.target hyprland-session.target";
    };
  };
}
