_:
{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.wayland.sway;
  cfgWayland = config.user-modules.desktop.wayland;
  cfgTheme = config.user-modules.themes;
  inherit (config.dotfiles) configDir;

in {
  options.user-modules.desktop.wayland.sway = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf (cfgWayland.enable && cfg.enable) {

    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemdIntegration = true;

      config = {
        bars = [{ command = "waybar"; }];

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
          # adaptive_sync = "on";
        };

        output."*" = { bg = "~/pics/walls/weebie/wallhaven-j3mmdy.jpg fill"; };

        modifier = "Mod4";
        terminal = "${pkgs.alacritty}/bin/alacritty";

        fonts = {
          names = [ cfgTheme.font.name ];
          # Sum required: floating point value but int option defined
          size = cfgTheme.font.size + 0.0;
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
              criteria = { app_id = "thunar"; };
            }
            {
              command = "floating enable";
              criteria = { app_id = "ipv"; };
            }
            {
              command = "floating enable";
              criteria = { app_id = "mpv"; };
            }
            {
              command = "floating enable position center, focus";
              criteria = { app_id = "GtkFileChooserDialog"; };
            }
            {
              command = "floating enable position center, focus";
              criteria = { app_id = "pop-up"; };
            }
            {
              command = "floating enable position center, focus";
              criteria = { app_id = "Organizer"; };
            }
            {
              command = "floating enable position center, focus";
              criteria = { app_id = "task_dialog"; };
            }
            {
              command = "floating enable position center, focus";
              criteria = { title = "^Polychromatic$"; };
            }
            {
              command = "floating enable";
              criteria = { app_id = "pavucontrol"; };
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
              criteria = { title = " — Sharing Indicator$"; };
            }
            {
              command = "shortcuts_inhibitor disable";
              criteria = { app_id = "^chrome-.*"; };
            }
          ];
        };

        floating = {
          modifier = "Mod4";
          border = 3;
        };

        startup = [
          { command = "corectrl"; }
          {
            command =
              "rm -f /tmp/sovpipe && mkfifo /tmp/sovpipe && tail -f /tmp/sovpipe | sov";
            always = true;
          }
          {
            command = "autotiling";
            always = true;
          }
          {
            command = "polychromatic-tray-applet";
          }
          # { command = "exec swhks & ; pkexec swhkd -c $HOME/.config/sway/swhkdrc -D 'IDOBAO ID80 Keyboard'"; }
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
          # I'm using sov with complex binds now
          # "${mod}+1" = "workspace 1";
          # "${mod}+2" = "workspace 2";
          # "${mod}+3" = "workspace 3";
          # "${mod}+4" = "workspace 4";
          # "${mod}+5" = "workspace 5";
          # "${mod}+6" = "workspace 6";
          # "${mod}+7" = "workspace 7";
          # "${mod}+8" = "workspace 8";
          # "${mod}+9" = "workspace 9";

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
          "${mod}+Shift+Right" =
            "move container to workspace next ; workspace next";
          "${mod}+Shift+Left" =
            " move container to workspace prev ; workspace prev";

          # Scratchpad
          "${mod}+minus" = "move scratchpad";
          "${mod}+equal" = "scratchpad show";

          # XF86
          "XF86AudioRaiseVolume" = "exec volume up";
          "XF86AudioLowerVolume" = "exec volume down";
          "XF86AudioMute" = "exec volume mute";

          # Launchers
          "${mod}+d" = "exec rofi_launcher";
          "${mod}+Shift+q" = "exec rofi_powermenu";
          "${mod}+slash" = "exec rofi_emoji";
          "${mod}+p" = "exec rofi-rbw";

          # Screenshots
          "Print" = "exec grimshot --notify copy";
          "Shift+Print" = "exec grimshot --notify save";
          "${mod}+Print" = "exec grimshot --notify copy area";
          "${mod}+Shift+Print" = "exec grimshot --notify save area";

          # Apps
          "${mod}+Return" = "exec ${term}";
          "${mod}+b" = "exec ${browser}";
          "${mod}+e" = "exec ${editor}";
          "${mod}+f" = "exec thunar";
        };
      };

      extraConfig = let
        mod = config.wayland.windowManager.sway.config.modifier;
        sovPipe = "/tmp/sovpipe";
      in ''
        bindsym --no-repeat ${mod}+1 workspace number 1; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+2 workspace number 2; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+3 workspace number 3; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+4 workspace number 4; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+5 workspace number 5; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+6 workspace number 6; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+7 workspace number 7; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+8 workspace number 8; exec "echo 1 > ${sovPipe}"
        bindsym --no-repeat ${mod}+9 workspace number 9; exec "echo 1 > ${sovPipe}"

        bindsym --release ${mod}+1 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+2 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+3 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+4 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+5 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+6 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+7 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+8 exec "echo 0 > ${sovPipe}"
        bindsym --release ${mod}+9 exec "echo 0 > ${sovPipe}"
      '';

      extraSessionCommands = ''
        export XDG_SESSION_TYPE=wayland
        export XDG_CURRENT_DESKTOP=sway
        export SDL_VIDEODRIVER=wayland
        export GTK_USE_PORTAL=1
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
        export MOZ_ENABLE_WAYLAND=1
        export _JAVA_AWT_WM_NONREPARENTING=1
        export NIXOS_OZONE_WL=1
      '';
    };

    # xdg.configFile."sway/swhkdrc".source = "${configDir}/sway/swhkdrc";

    xdg.configFile."sov/config".source = "${configDir}/sway/sov";

    home.packages = with pkgs; [ gsettings-desktop-schemas autotiling sov ];

    user-modules.desktop = {
      services = {
        dunst.enable = true;
        locker-wayland.enable = true;
        waybar.enable = true;
      };
    };
  };
}
