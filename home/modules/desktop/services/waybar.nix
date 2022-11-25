{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.services.waybar;
  cfgTheme = config.mario.modules.themes;
  inherit (config.colorScheme) colors;

  # Function to simplify making waybar outputs
  # https://github.com/Misterio77/nix-config/blob/main/home/misterio/features/desktop/common/wayland-wm/waybar.nix
  jsonOutput = name: {
    pre ? "",
    text ? "",
    tooltip ? "",
    alt ? "",
    class ? "",
    percentage ? "",
  }: "${pkgs.writeShellScriptBin "waybar-${name}" ''
    set -euo pipefail
    ${pre}
    ${pkgs.jq}/bin/jq -cn \
      --arg text "${text}" \
      --arg tooltip "${tooltip}" \
      --arg alt "${alt}" \
      --arg class "${class}" \
      --arg percentage "${percentage}" \
      '{text:$text,tooltip:$tooltip,alt:$alt,class:$class,percentage:$percentage}'
  ''}/bin/waybar-${name}";
in {
  options.mario.modules.desktop.services.waybar = {
    enable = mkEnableOption "waybar configuration";
  };

  config = mkIf cfg.enable {
    programs.waybar = {
      enable = true;
      package = pkgs.waybar.override {pulseSupport = true;};
      settings = [
        {
          height = 36;
          layer = "top";

          modules-left =
            [
              "custom/menu"
              "idle_inhibitor"
            ]
            ++ (optionals config.wayland.windowManager.sway.enable [
              "sway/workspaces"
              "sway/mode"
            ])
            ++ (optionals config.wayland.windowManager.hyprland.enable [
              "wlr/workspaces"
            ]);

          modules-center = ["clock"];

          modules-right = ["tray" "temperature" "cpu" "memory" "pulseaudio" "custom/hostname"];

          "sway/workspaces" = {
            all-outputs = true;
            format = "{icon}";
            format-icons = {
              "1" = "1";
              "2" = "2";
              "3" = "3";
              "4" = "4";
              "5" = "5";
              "6" = "6";
              "7" = "7";
              "8" = "8";
              "9" = "9";
            };
          };

          "wlr/workspaces" = {
            on-click = "activate";
            on-scroll-up = "hyprctl dispatch workspace e+1";
            on-scroll-down = "hyprctl dispatch workspace e-1";
          };

          idle_inhibitor = {
            format = "{icon}";
            format-icons = {
              activated = "零";
              deactivated = "鈴";
            };
          };

          "custom/hostname" = {
            exec = "echo $USER@$(hostname)";
            on-click = "alacritty";
          };

          "custom/menu" = {
            return-type = "json";
            exec = jsonOutput "menu" {
              text = "";
              tooltip = ''$(cat /etc/os-release | grep PRETTY_NAME | cut -d '"' -f2)'';
            };
            on-click = "rofi-powermenu";
          };

          tray = {
            icon-size = 24;
            spacing = 10;
          };

          clock = {
            format = "󰥔 {:%H:%M}";
            format-alt = "{:%A, %d %b}";
            tooltip-format = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>'';
          };

          cpu = {
            format = "󰻠 {usage}% ";
            interval = 10;
          };

          temperature = {
            hwmon-path = "/sys/class/hwmon/hwmon3/temp1_input";
            format = "󰔏 {temperatureC}°C";
            format-critical = "󱃂 {temperatureC}°C";
            critical-threshold = 80;
            interval = 10;
          };

          memory = {
            format = "󰍛 {used:0.1f}GB/{total:0.1f}GB ";
            interval = 10;
          };

          pulseaudio = {
            scroll-step = 1;
            format = "{icon} {volume}% {format_source}";
            format-bluetooth = "{icon}󰂯 {volume}% {format_source}";
            format-bluetooth-muted = "{icon}󰂲 {format_source}";
            format-muted = "󰖁 {format_source}";
            format-source = "󰍬 {volume}%";
            format-source-muted = "󰍭";
            format-icons = {
              headphone = "󰋋";
              hands-free = "";
              headset = "󰋎";
              phone = "󰄜";
              portable = "󰄜";
              car = "󰄋";
              default = ["󰕿" "󰖀" "󰕾"];
            };
            on-click = "pavucontrol";
          };
        }
      ];

      style = ''
        * {
            border: none;
            border-radius: 0;
            font-family: "Iosevka Nerd Font";
            font-size: 20px;
            font-weight: normal;
        }

        #custom-menu {
           background-color: #${colors.base0C};
            color: #${colors.base00};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 22px;
            border-radius: 14px;
        }

        #custom-hostname {
            background-color: #${colors.base0C};
            color: #${colors.base00};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 5px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 14px;
            border-radius: 14px;
        }

        tooltip {
            background-color: #${colors.base00};
            color: #${colors.base07};
            border-radius: 10px;
            padding: 4px;
        }

        window#waybar {
            background-color: #${colors.base00};
            color: #${colors.base07};
            transition-property: background-color;
            transition-duration: 0.5s;
        }

        #idle_inhibitor {
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 22px;
            border-radius: 14px;
        }

        #workspaces {
            margin-left: 4px;
            margin-right: 4px;
        }

        #workspaces button {
            background: #${colors.base02};
            color: #${colors.base07};
            padding: 0 8px;
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            border-radius: 14px;
            transition-duration: 0.2s;
        }

        #workspaces button:hover {
            background-color: #${colors.base03};
            color: #${colors.base07};
            transition-duration: 0.2s;
        }

        #workspaces button.active {
            background-color: #${colors.base0B};
            color: #${colors.base00};
            padding: 0 12px;
            transition-duration: 0.2s;
        }

        #workspaces button.focused {
            background-color: #${colors.base0B};
            color: #${colors.base00};
            padding: 0 12px;
            transition-duration: 0.2s;
        }

        #workspaces button.urgent {
            background: #${colors.base08};
            color: #${colors.base00};
        }

        #tray,
        #cpu,
        #temperature,
        #memory,
        #pulseaudio,
        #clock {
            padding: 0 10px;
            color: #${colors.base07};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 5px;
            margin-right: 5px;
            border-radius: 14px;
        }

        /* Tray */
        #tray {
            background-color: #${colors.base02};
            color: #${colors.base00};
        }

        #tray > .passive {
            -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
            -gtk-icon-effect: highlight;
            background-color: #eb4d4b;
        }

        /* CPU */
        #cpu {
            background-color: #${colors.base0C};
            color: #${colors.base00};
        }

        /* Temperature */
        #temperature {
            background-color: #${colors.base09};
            color: #${colors.base00};
        }

        /* Memory */
        #memory {
            background-color: #${colors.base0B};
            color: #${colors.base00};
        }

        /* Audio */
        #pulseaudio {
            background-color: #${colors.base0E};
            color: #${colors.base00};
        }

        /* Clock */
        #clock {
            background-color: #${colors.base0D};
            color: #${colors.base00};
        }
      '';
    };
  };
}
