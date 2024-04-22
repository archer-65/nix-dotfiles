{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.wayland.waybar;
  cfgTheme = config.mario.modules.themes;
  inherit (config.colorScheme) palette;

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
  options.mario.modules.wayland.waybar = {
    enable = mkEnableOption "waybar configuration";

    battery = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    temperature = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
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
              "hyprland/workspaces"
              "hyprland/submap"
            ]);

          modules-center = ["clock"];

          modules-right =
            [
              "tray"
              "temperature"
              "cpu"
              "memory"
              "pulseaudio"
            ]
            ++ (optionals (cfg.battery != null) [
              "battery"
            ])
            ++ [
              "custom/hostname"
            ];

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

          "hyprland/workspaces" = {
            on-click = "activate";
            on-scroll-up = "hyprctl dispatch workspace e+1";
            on-scroll-down = "hyprctl dispatch workspace e-1";
          };

          idle_inhibitor = {
            format = "{icon}";
            format-icons = {
              activated = "󰒳";
              deactivated = "󰒲";
            };
          };

          "custom/hostname" = {
            exec = "echo $(hostname)";
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

          # FIXME: Hardcoded config
          clock = {
            max-length = 25;
            format = "󰥔 {:%H:%M}";
            format-alt = "{:%A, %d %b}";
            tooltip-format = "<span font='IBM Plex Mono 15'><tt><small>{calendar}</small></tt></span>";
            calendar = {
              mode = "month";
              mode-mon-col = 3;
              on-scroll = 1;
              format = {
                months = "<span color='#ffead3'><b>{}</b></span>";
                days = "<span color='#ecc6d9'><b>{}</b></span>";
                weeks = "<span color='#99ffdd'><b>W{}</b></span>";
                weekdays = "<span color='#ffcc66'><b>{}</b></span>";
                today = "<span color='#ff6699'><b><u>{}</u></b></span>";
              };
            };
            actions = {
              on-click-right = "mode";
              on-click-forward = "tz_up";
              on-click-backward = "tz_down";
              on-scroll-up = "shift_up";
              on-scroll-down = "shift_down";
            };
          };

          cpu = {
            format = "󰻠 {usage}% ";
            interval = 10;
          };

          temperature = {
            hwmon-path = "${cfg.temperature}";
            format = "󰔏 {temperatureC}°C";
            format-critical = "󱃂 {temperatureC}°C";
            critical-threshold = 80;
            interval = 10;
          };

          memory = {
            format = "󰍛 {used:0.1f}GB/{total:0.1f}GB ";
            interval = 10;
          };

          battery = optionalAttrs (cfg.battery != null) {
            bat = "${cfg.battery}";
            interval = 30;
            format-icons = ["󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
            format = "{icon} {capacity}%";
            format-charging = "󰂄 {capacity}%";
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
            font-family: "${cfgTheme.font.bar.family}";
            font-size: ${toString cfgTheme.font.bar.size}px;
            font-weight: normal;
        }

        #custom-menu {
           background-color: #${palette.base0C};
            color: #${palette.base00};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 22px;
            border-radius: 14px;
        }

        #custom-hostname {
            background-color: #${palette.base0C};
            color: #${palette.base00};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 5px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 14px;
            border-radius: 14px;
        }

        tooltip {
            background-color: #${palette.base00};
            color: #${palette.base07};
            border-radius: 10px;
            padding: 4px;
        }

        window#waybar {
            background-color: #${palette.base00};
            color: #${palette.base07};
            transition-property: background-color;
            transition-duration: 0.5s;
        }

        #idle_inhibitor {
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 12px;
            border-radius: 14px;
        }

        #workspaces {
            margin-left: 4px;
            margin-right: 4px;
        }

        #workspaces button {
            background: #${palette.base02};
            color: #${palette.base07};
            padding: 0 8px;
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            border-radius: 14px;
            transition-duration: 0.2s;
        }

        #workspaces button:hover {
            background-color: #${palette.base03};
            color: #${palette.base07};
            transition-duration: 0.2s;
        }

        #workspaces button.active {
            background-color: #${palette.base0B};
            color: #${palette.base00};
            padding: 0 12px;
            transition-duration: 0.2s;
        }

        #workspaces button.focused {
            background-color: #${palette.base0B};
            color: #${palette.base00};
            padding: 0 12px;
            transition-duration: 0.2s;
        }

        #workspaces button.urgent {
            background: #${palette.base08};
            color: #${palette.base00};
        }

        #tray,
        #cpu,
        #temperature,
        #memory,
        #battery,
        #pulseaudio,
        #clock {
            padding: 0 10px;
            color: #${palette.base07};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 5px;
            margin-right: 5px;
            border-radius: 14px;
        }

        /* Tray */
        #tray {
            background-color: #${palette.base02};
            color: #${palette.base00};
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
            background-color: #${palette.base0C};
            color: #${palette.base00};
        }

        /* Temperature */
        #temperature {
            background-color: #${palette.base09};
            color: #${palette.base00};
        }

        /* Memory */
        #memory {
            background-color: #${palette.base0B};
            color: #${palette.base00};
        }

        /* Battery */
        #battery {
            background-color: #${palette.base0A};
            color: #${palette.base00};
        }

        /* Audio */
        #pulseaudio {
            background-color: #${palette.base0E};
            color: #${palette.base00};
        }

        /* Clock */
        #clock {
            background-color: #${palette.base0D};
            color: #${palette.base00};
        }
      '';
    };
  };
}
