_:
{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.services.waybar;
  cfgTheme = config.user-modules.themes;
  colors = config.colorScheme.colors;
in {
  options.user-modules.desktop.services.waybar = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.waybar = {
      enable = true;
      package = pkgs.waybar.override { pulseSupport = true; };
      settings = [{
        height = 36;

        modules-left = [
          "sway/workspaces"
        ];
        
        modules-center = [
          "clock"
        ];
        
        modules-right = [
          "tray"
          "cpu"
          "temperature"
          "memory"
          "pulseaudio"
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
          hwmon-path =
            "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon3/temp1_input";
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
            default = [ "󰕿" "󰖀" "󰕾" ];
          };
          on-click = "pavucontrol";
        };

      }];

      style = ''
               * {
                   border: none;
                   border-radius: 0;
                   font-family: "Iosevka", "Material Design Icons";
                   font-size: 22px;
                   font-weight: normal;
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

               #workspaces {
                   margin-left: 2px;
                   margin-right: 2px;
               }

               #workspaces button {
                   background: #${colors.base02};
                   color: #${colors.base07};
                   padding: 1px 4px;
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
                   padding: 1px 6px;
                   transition-duration: 0.2s;
               }

               #workspaces button.focused {
                   background-color: #${colors.base0B};
                   color: #${colors.base00};
                   padding: 1px 6px;
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
                   padding: 2px 10px;
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
