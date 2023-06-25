{
  config,
  lib,
  pkgs,
  outputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.wayland;
  cfgTheme = config.mario.modules.themes;
in {
  config = mkIf (cfg.enable && (elem "sway" cfg.wm)) {
    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemd.enable = true;

      config = {
        bars = [{command = "waybar";}];

        input."type:keyboard" = {
          xkb_layout = "us";
          xkb_variant = "altgr-intl";
          xkb_options = "ctrl:nocaps";
        };

        input."1390:268:ELECOM_TrackBall_Mouse_HUGE_TrackBall" = {
          scroll_method = "on_button_down";
          scroll_button = "BTN_TASK";
        };

        output."Unknown U34G2G4R3 0x0000241D" = {
          mode = "3440x1440@144.001Hz";
        };

        output."*" = {bg = "${outputs.wallpapers.city-lights.src} fill";};

        modifier = "Mod4";
        terminal = "${pkgs.alacritty}/bin/alacritty";

        fonts = {
          names = [cfgTheme.font.regular.family];
          # Sum required: floating point value but int option defined
          size = cfgTheme.font.regular.size + 0.0;
        };

        gaps.inner = 10;
        gaps.outer = 0;

        focus.followMouse = true;

        window = {
          titlebar = false;
          border = 4;

          commands = [
            {
              command = "floating enable";
              criteria = {app_id = "thunar";};
            }
            {
              command = "floating enable";
              criteria = {app_id = "ipv";};
            }
            {
              command = "floating enable";
              criteria = {app_id = "mpv";};
            }
            {
              command = "floating enable position center, focus";
              criteria = {app_id = "GtkFileChooserDialog";};
            }
            {
              command = "floating enable position center, focus";
              criteria = {app_id = "pop-up";};
            }
            {
              command = "floating enable position center, focus";
              criteria = {app_id = "Organizer";};
            }
            {
              command = "floating enable position center, focus";
              criteria = {app_id = "task_dialog";};
            }
            {
              command = "floating enable position center, focus";
              criteria = {title = "^Polychromatic$";};
            }
            {
              command = "floating enable";
              criteria = {app_id = "pavucontrol";};
            }
            {
              command = "floating enable, sticky enable";
              criteria = {
                app_id = "firefox";
                title = "^Picture-in-Picture$";
              };
            }
            {
              command = "floating enable, sticky enable, border none, nofocus";
              criteria = {title = " — Sharing Indicator$";};
            }
            {
              command = "shortcuts_inhibitor disable";
              criteria = {app_id = "^chrome-.*";};
            }
          ];
        };

        floating = {
          modifier = "Mod4";
          border = 3;
        };

        startup = [
          {command = "corectrl";}
          {
            command = "autotiling";
            always = true;
          }
          {
            command = "polychromatic-tray-applet";
          }
          {
            command = "exec wl-paste -t text --watch clipman store --no-persist";
          }
          {
            command = "exec wl-paste -p -t text --watch clipman store -P --histpath=\"~/.local/share/clipman-primary.json\"";
          }
        ];

        keybindings = let
          mod = config.wayland.windowManager.sway.config.modifier;
          term = config.wayland.windowManager.sway.config.terminal;
          browser = "firefox";
          editor = "emacsclient -c";
          fm = "thunar";
        in {
          # Base
          "${mod}+ctrl+r" = "reload";
          "${mod}+Shift+r" = "restart";

          # Windows
          "${mod}+w" = "kill";

          # Focus
          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";

          # Cycle between tiling/floating
          "${mod}+Shift+t" = "focus mode_toggle";
          "${mod}+t" = "floating toggle";

          # Parent-child
          "${mod}+bracketleft" = "focus parent";
          "${mod}+bracketright" = "child";

          # Moving windows
          "${mod}+Shift+h" = "move left";
          "${mod}+Shift+j" = "move down";
          "${mod}+Shift+k" = "move up";
          "${mod}+Shift+l" = "move right";

          # Resize windows
          "${mod}+ctrl+h" = "resize shrink width 5 px or 5 ppt";
          "${mod}+ctrl+j" = "resize grow height 5 px or 5 ppt";
          "${mod}+ctrl+k" = "resize shrink height 5 px or 5 ppt";
          "${mod}+ctrl+l" = "resize grow width 5 px or 5 ppt";

          # Change layout
          "${mod}+o" = "split toggle";
          "${mod}+Tab" = "layout toggle all";
          "F11" = "fullscreen toggle";

          # Gaps
          "${mod}+ctrl+equal" = "gaps inner all plus 5";
          "${mod}+ctrl+minus" = "gaps inner all minus 5";

          # Workspaces
          "${mod}+1" = "workspace 1";
          "${mod}+2" = "workspace 2";
          "${mod}+3" = "workspace 3";
          "${mod}+4" = "workspace 4";
          "${mod}+5" = "workspace 5";
          "${mod}+6" = "workspace 6";
          "${mod}+7" = "workspace 7";
          "${mod}+8" = "workspace 8";
          "${mod}+9" = "workspace 9";

          # Switch to next/previous ws
          "${mod}+Right" = "workspace next";
          "${mod}+Left" = "workspace prev";

          # Move to ws and switch to ws
          "${mod}+Shift+1" = "move container to workspace 1  ; workspace 1";
          "${mod}+Shift+2" = "move container to workspace 2  ; workspace 2";
          "${mod}+Shift+3" = "move container to workspace 3  ; workspace 3";
          "${mod}+Shift+4" = "move container to workspace 4  ; workspace 4";
          "${mod}+Shift+5" = "move container to workspace 5  ; workspace 5";
          "${mod}+Shift+6" = "move container to workspace 6  ; workspace 6";
          "${mod}+Shift+7" = "move container to workspace 7  ; workspace 7";
          "${mod}+Shift+8" = "move container to workspace 8  ; workspace 8";
          "${mod}+Shift+9" = "move container to workspace 9  ; workspace 9";

          # Move to next/previous and switch
          "${mod}+Shift+Right" = "move container to workspace next ; workspace next";
          "${mod}+Shift+Left" = " move container to workspace prev ; workspace prev";

          # Scratchpad
          "${mod}+minus" = "move scratchpad";
          "${mod}+equal" = "scratchpad show";

          # XF86
          "XF86AudioRaiseVolume" = "exec pamixer -u && pamixer -i 5";
          "XF86AudioLowerVolume" = "exec pamixer -u && pamixer -d 5";
          "XF86AudioMute" = "exec pamixer -t";

          # Launchers
          "${mod}+d" = "exec rofi -no-lazy-grab -show drun -modi run,drun -theme $HOME/.config/rofi/themes/launcher";
          "${mod}+Shift+q" = "exec rofi-powermenu";
          "${mod}+slash" = "exec rofi -show emoji -modi emoji -theme $HOME/.config/rofi/themes/emoji";
          "${mod}+p" = "exec rofi-rbw";

          # Clipboard
          "${mod}+comma" = "clipman pick -t rofi -T'-theme ~/.config/rofi/themes/clipboard'";

          # Screenshots
          "Print" = "exec grimshot --notify copy";
          "Shift+Print" = "exec grimshot --notify save";
          "${mod}+Print" = "exec grimshot --notify copy area";
          "${mod}+Shift+Print" = "exec grimshot --notify save area";

          # Apps
          "${mod}+Return" = "exec ${term}";
          "${mod}+b" = "exec ${browser}";
          "${mod}+e" = "exec ${editor}";
          "${mod}+f" = "exec ${fm}";
        };
      };
    };

    home.packages = with pkgs; [autotiling];

    mario.modules = {
      apps.dunst.enable = true;
      wayland = {
        locker.enable = true;
        waybar.enable = true;
      };
    };
  };
}
