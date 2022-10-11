_:
{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.services.waybar;
  cfgTheme = config.user-modules.themes;
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
        height = 35;

        modules-left = [
          "sway/workspaces"
          "sway/mode"
          # "custom/media#0" "custom/media#1"
        ];
        modules-center = [
          "clock"
        ];
        modules-right = [
          # "network"
          "tray"
          "cpu"
          "temperature"
          "memory"
          "pulseaudio"
          #"custom/power"
        ];

        "sway/workspaces" = {
          all-outputs = true;
          format = "{icon}";
          format-icons = {
            "1" = "一";
            "2" = "二";
            "3" = "三";
            "4" = "四";
            "5" = "五";
            "6" = "六";
            "7" = "七";
            "8" = "八";
            "9" = "九";
          };

          persistent_workspaces = {
            "1" = "[]";
            "2" = "[]";
            "3" = "[]";
            "4" = "[]";
            "5" = "[]";
            "6" = "[]";
            "7" = "[]";
            "8" = "[]";
            "9" = "[]";
          };
        };

        tray = {
          icon-size = 24;
          spacing = 10;
        };

        clock = {
          tooltip-format = ''
            <big>{:%Y %B}</big>
            <tt><small>{calendar}</small></tt>'';
          format-alt = "{:%A, %d %b}";
        };

        cpu = {
          format = " {usage}% ";
          interval = 10;
        };

        temperature = {
          hwmon-path =
            "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon3/temp1_input";
          format = " {temperatureC}°C";
          format-critical = " {temperatureC}°C";
          critical-threshold = 80;
          interval = 10;
        };

        memory = {
          format = " {}% ";
          interval = 10;
        };

        pulseaudio = {
          scroll-step = 1;
          format = "{icon} {volume}% {format_source}";
          format-bluetooth = "{icon} {volume}% {format_source}";
          format-bluetooth-muted = " {icon} {format_source}";
          format-muted = " {format_source}";
          format-source = " {volume}%";
          format-source-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" "" ];
          };
          on-click = "pavucontrol";
        };

      }];

      style = ''
               /* Colors */
               #workspaces button,
               #mode,
               #tray,
               #pulseaudio,
               #memory,
               #cpu,
               #temperature,
               #clock {
                   background: rgba(229, 231, 235, 0.9);
                   color: #374151;
               }

               /* Effects */
               #workspaces button:hover {
                   background: rgba(229, 231, 235, 0.4);
               }
               #workspaces button.focused {
                   background: #9CA3AF;
                   color: #1F2937;
               }
               /* #workspaces button.urgent {
                   background: 
               } */

               * {
                   border: none;
                   border-radius: 0;
                   font-family: "Roboto", "Noto Sans CJK JP", "Font Awesome 6 Free";
                   font-size: 20px;
                   font-weight: normal;
               }

               window#waybar {
                   background-color: rgba(0,0,0,0);
               }

               window#waybar.hidden {
                   opacity: 0.2;
               }

               #waybar > .horizontal {
                   padding: 5px 10px 0;
               }

               #waybar > .horizontal > .horizontal:nth-child(1) {
                   margin-right: 10px;
               }

               #workspaces button {
               	  margin: 0px 0 0 5px;
               	  font-size: 16px;
               	  padding: 5px 2px;
               	  border-radius: 5px;
               }

               #workspaces button:hover {
                   box-shadow: inherit;
                   text-shadow: inherit;
               }

               #mode {
               	  margin: 10px 0 0 10px;
               	  padding: 0 10px;
               	  border-radius: 5px;
               }

               #window {
                  font-weight: 600;
                  margin: 10px 0 0 10px;
               }

               #tray,
               #pulseaudio,
               #network,
               #memory,
               #temperature,
               #cpu,
               #clock {
                   margin: 0px 10px 0 0;
        	         padding: 7px 10px;
                   border-radius: 5px;
               }           
      '';
    };
  };
}
