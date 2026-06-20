# NOTE: Migration from Hyprlang to Lua (2026-06).
#
# This is a pretty ugly note that should be migrated somewhere else.
#
# Hyprland deprecated the legacy config format and will remove support.
# HM PR #9307 added Lua generation — we use `configType = "lua"` with
# structured Nix attrsets that produce Lua config. This keeps Nix as
# the single source of truth while future-proofing for a direct Lua config.
#
# References (keep until full-Lua decision):
# - https://github.com/nix-community/home-manager/pull/9307
# - https://github.com/khaneliman/khanelinix/blob/main/modules/home/programs/graphical/wms/hyprland
# - https://github.com/rwxae/dotfiles/blob/main/home/linux/hyprland.nix
# - https://www.reddit.com/r/hyprland/comments/1t8ykjm/just_finished_moving_my_hypr_configs_to_lua/
# - https://www.reddit.com/r/NixOS/comments/1tg9cse/hyprland_hm_lua_config_migration/
# - https://www.reddit.com/r/hyprland/comments/1tl1ked/your_hyprlandconf_will_stop_working_heres_the/
#
# TODO: Make a decision for the long term, options are simple:
# A. Optimize the config for Nix by having utility functions
# B. Migrate to Lua
#
# Rationale for B would be dropping the abstraction layer, I don't really get the benefit.
{
  config,
  lib,
  pkgs,
  outputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.wayland;
  mkLuaInline = lib.generators.mkLuaInline;
in {
  config = mkIf (cfg.enable && (elem "hyprland" cfg.wm)) {
    stylix.targets.hyprland.enable = true;

    wayland.windowManager.hyprland = {
      enable = true;
      configType = "lua";
      package = pkgs.inputs.hyprland.hyprland;
      portalPackage = pkgs.inputs.hyprland.xdg-desktop-portal-hyprland;

      xwayland.enable = true;

      systemd = {
        enable = true;
        variables = [
          "DISPLAY"
          "HYPRLAND_INSTANCE_SIGNATURE"
          "WAYLAND_DISPLAY"
          "XDG_CURRENT_DESKTOP"
          "PATH"
        ];
      };

      settings = let
        terminal = "${pkgs.alacritty}/bin/alacritty";
        browser = "${pkgs.firefox}/bin/firefox";
        editor = "emacsclient -c";
        fm = "thunar";
      in {
        config = {
          general = {
            gaps_in = 5;
            gaps_out = 5;
            border_size = 2;
            layout = "master";
          };

          decoration = {
            active_opacity = 1.0;
            inactive_opacity = 0.98;
            fullscreen_opacity = 1.0;
            rounding = 5;
            blur.enabled = false;
          };

          animations = {
            enabled = true;
          };

          master = {
            new_status = "slave";
            new_on_top = false;
            special_scale_factor = 0.85;
          };

          dwindle = {
            preserve_split = true;
            special_scale_factor = 0.85;
          };

          misc = {
            enable_swallow = true;
            swallow_regex = "^(Alacritty)$";
          };

          input = {
            kb_layout = "us";
            kb_variant = "altgr-intl";
            kb_options = "ctrl:nocaps";
            accel_profile = "flat";
            follow_mouse = 1;
            repeat_delay = 300;
            repeat_rate = 50;
          };
        };

        animation = [
          {
            leaf = "windows";
            enabled = true;
            speed = 4;
            bezier = "default";
            style = "slide";
          }
          {
            leaf = "border";
            enabled = true;
            speed = 5;
            bezier = "default";
          }
          {
            leaf = "fade";
            enabled = true;
            speed = 0.1;
            bezier = "default";
          }
          {
            leaf = "workspaces";
            enabled = true;
            speed = 2;
            bezier = "default";
            style = "fade";
          }
        ];

        monitor = [
          {
            output = "desc:AOC U34G2G4R3 0x0000241D";
            mode = "3440x1440@144";
            position = "0x0";
            scale = 1;
          }
        ];

        gesture = {
          fingers = 3;
          direction = "horizontal";
          action = "workspace";
        };

        device = {
          name = "elecom-trackball-mouse-huge-trackball-1";
          scroll_method = "on_button_down";
          scroll_button = 279;
          accel_profile = "adaptive";
        };

        window_rule = [
          {
            match.class = "^(thunar|Thunar)$";
            float = true;
            persistent_size = true;
          }
          {
            match.class = "^(ipv)$";
            float = true;
          }
          {
            match.class = "^(mpv)$";
            float = true;
          }
          {
            match.class = "^(pavucontrol)$";
            float = true;
          }
          {
            match.title = "^(Polychromatic)$";
            float = true;
          }
          {
            match.class = "^(GtkFileChooserDialog)$";
            float = true;
          }
          {
            match.class = "^(pop-up)$";
            float = true;
          }
          {
            match.class = "^(Organizer)$";
            float = true;
          }
          {
            match.class = "(task_dialog)$";
            float = true;
          }
          {
            match.class = "^(firefox)$";
            match.title = "^(Picture-in-Picture)$";
            float = true;
          }
          {
            match.class = "^(firefox)$";
            match.title = "^(Picture-in-Picture)$";
            pin = true;
          }
          {
            match.class = "^(firefox)$";
            match.title = "^Firefox — Sharing Indicator$";
            workspace = "special:trash";
          }
          {
            match.class = "^(mpv)$";
            idle_inhibit = "focus";
          }
          {
            match.class = "^(firefox)$";
            idle_inhibit = "fullscreen";
          }
        ];

        exec_cmd = [

        ];

        on = {
          # TODO: Migrate what makes sense to systemd service instead
          # TODO: At this point in time, may be better to switch to hyprpaper instead
          _args = [
            "hyprland.start"
            (mkLuaInline ''
              function()
                hl.exec_cmd("corectrl")
                hl.exec_cmd("polychromatic-tray-applet")
                hl.exec_cmd("${pkgs.swaybg}/bin/swaybg -i ${outputs.wallpapers.digital-flowers.src} --mode fill")
              end
            '')
          ];
        };

        bind = [
          # Group navigation
          {
            _args = [
              "SUPER + g"
              (mkLuaInline "hl.dsp.group.toggle()")
            ];
          }
          {
            _args = [
              "SUPER + apostrophe"
              (mkLuaInline "hl.dsp.group.next()")
            ];
          }
          {
            _args = [
              "SUPER + SHIFT + apostrophe"
              (mkLuaInline "hl.dsp.group.prev()")
            ];
          }

          # Master layout
          {
            _args = [
              "SUPER + m"
              (mkLuaInline "hl.dsp.layout(\"focusmaster\")")
            ];
          }
          {
            _args = [
              "SUPER + SHIFT + m"
              (mkLuaInline "hl.dsp.layout(\"swapwithmaster\")")
            ];
          }
          {
            _args = [
              "SUPER + down"
              (mkLuaInline "hl.dsp.window.cycle_next()")
            ];
          }
          {
            _args = [
              "SUPER + up"
              (mkLuaInline "hl.dsp.window.cycle_next({ next = false })")
            ];
          }
          {
            _args = [
              "SUPER + SHIFT + down"
              (mkLuaInline "hl.dsp.window.swap({ next = true })")
            ];
          }
          {
            _args = [
              "SUPER + SHIFT + up"
              (mkLuaInline "hl.dsp.window.swap({ prev = true })")
            ];
          }

          # Toggle bar
          {
            _args = [
              "SUPER + CTRL + F1"
              (mkLuaInline "hl.dsp.exec_cmd(\"${pkgs.procps}/bin/pkill -USR1 waybar\")")
            ];
          }
          {
            _args = [
              "SUPER + SHIFT + F1"
              (mkLuaInline "hl.dsp.exec_cmd(\"${pkgs.procps}/bin/pkill waybar && waybar\")")
            ];
          }

          # Layout switching
          {
            _args = [
              "SUPER + CTRL + d"
              (mkLuaInline "function() hl.config({ general = { layout = \"dwindle\" } }) end")
            ];
          }
          {
            _args = [
              "SUPER + CTRL + m"
              (mkLuaInline "function() hl.config({ general = { layout = \"master\" } }) end")
            ];
          }

          # Workspace switch
          {
            _args = [ "SUPER + 1" (mkLuaInline "hl.dsp.focus({ workspace = \"1\" })") ];
          }
          {
            _args = [ "SUPER + 2" (mkLuaInline "hl.dsp.focus({ workspace = \"2\" })") ];
          }
          {
            _args = [ "SUPER + 3" (mkLuaInline "hl.dsp.focus({ workspace = \"3\" })") ];
          }
          {
            _args = [ "SUPER + 4" (mkLuaInline "hl.dsp.focus({ workspace = \"4\" })") ];
          }
          {
            _args = [ "SUPER + 5" (mkLuaInline "hl.dsp.focus({ workspace = \"5\" })") ];
          }
          {
            _args = [ "SUPER + 6" (mkLuaInline "hl.dsp.focus({ workspace = \"6\" })") ];
          }
          {
            _args = [ "SUPER + 7" (mkLuaInline "hl.dsp.focus({ workspace = \"7\" })") ];
          }
          {
            _args = [ "SUPER + 8" (mkLuaInline "hl.dsp.focus({ workspace = \"8\" })") ];
          }
          {
            _args = [ "SUPER + 9" (mkLuaInline "hl.dsp.focus({ workspace = \"9\" })") ];
          }

          # Move to workspace
          {
            _args = [ "SUPER + SHIFT + 1" (mkLuaInline "hl.dsp.window.move({ workspace = \"1\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 2" (mkLuaInline "hl.dsp.window.move({ workspace = \"2\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 3" (mkLuaInline "hl.dsp.window.move({ workspace = \"3\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 4" (mkLuaInline "hl.dsp.window.move({ workspace = \"4\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 5" (mkLuaInline "hl.dsp.window.move({ workspace = \"5\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 6" (mkLuaInline "hl.dsp.window.move({ workspace = \"6\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 7" (mkLuaInline "hl.dsp.window.move({ workspace = \"7\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 8" (mkLuaInline "hl.dsp.window.move({ workspace = \"8\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + 9" (mkLuaInline "hl.dsp.window.move({ workspace = \"9\" })") ];
          }

          # Focus direction
          {
            _args = [ "SUPER + h" (mkLuaInline "hl.dsp.focus({ direction = \"l\" })") ];
          }
          {
            _args = [ "SUPER + j" (mkLuaInline "hl.dsp.focus({ direction = \"d\" })") ];
          }
          {
            _args = [ "SUPER + k" (mkLuaInline "hl.dsp.focus({ direction = \"u\" })") ];
          }
          {
            _args = [ "SUPER + l" (mkLuaInline "hl.dsp.focus({ direction = \"r\" })") ];
          }

          # Move window direction
          {
            _args = [ "SUPER + SHIFT + h" (mkLuaInline "hl.dsp.window.move({ direction = \"l\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + j" (mkLuaInline "hl.dsp.window.move({ direction = \"d\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + k" (mkLuaInline "hl.dsp.window.move({ direction = \"u\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + l" (mkLuaInline "hl.dsp.window.move({ direction = \"r\" })") ];
          }

          # Workspace navigation
          {
            _args = [ "SUPER + left"  (mkLuaInline "hl.dsp.focus({ workspace = \"-1\" })") ];
          }
          {
            _args = [ "SUPER + right" (mkLuaInline "hl.dsp.focus({ workspace = \"+1\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + left"  (mkLuaInline "hl.dsp.window.move({ workspace = \"-1\" })") ];
          }
          {
            _args = [ "SUPER + SHIFT + right" (mkLuaInline "hl.dsp.window.move({ workspace = \"+1\" })") ];
          }

          # Monitor focus
          {
            _args = [ "SUPER + bracketleft" (mkLuaInline "hl.dsp.focus({ monitor = \"l\" })") ];
          }
          {
            _args = [ "SUPER + bracketright" (mkLuaInline "hl.dsp.focus({ monitor = \"r\" })") ];
          }

          # Special workspace
          {
            _args = [ "SUPER + SHIFT + backslash" (mkLuaInline "hl.dsp.window.move({ workspace = \"special\" })") ];
          }
          {
            _args = [ "SUPER + backslash" (mkLuaInline "hl.dsp.workspace.toggle_special()") ];
          }

          # Resize active
          {
            _args = [ "SUPER + CTRL + h" (mkLuaInline "hl.dsp.window.resize({ x = -50, y = 0, relative = true })") ];
          }
          {
            _args = [ "SUPER + CTRL + j" (mkLuaInline "hl.dsp.window.resize({ x = 0, y = -50, relative = true })") ];
          }
          {
            _args = [ "SUPER + CTRL + k" (mkLuaInline "hl.dsp.window.resize({ x = 0, y = 50, relative = true })") ];
          }
          {
            _args = [ "SUPER + CTRL + l" (mkLuaInline "hl.dsp.window.resize({ x = 50, y = 0, relative = true })") ];
          }

          # Window ops
          {
            _args = [ "SUPER + w" (mkLuaInline "hl.dsp.window.close()") ];
          }
          {
            _args = [ "SUPER + q" (mkLuaInline "hl.dsp.window.kill()") ];
          }
          {
            _args = [ "SUPER + t" (mkLuaInline "hl.dsp.window.float()") ];
          }
          {
            _args = [ "F11" (mkLuaInline "hl.dsp.window.fullscreen({ mode = \"maximized\" })") ];
          }
          {
            _args = [ "SUPER + F11" (mkLuaInline "hl.dsp.window.fullscreen({ mode = \"fullscreen\" })") ];
          }

          # Media keys
          {
            _args = [ "XF86AudioRaiseVolume" (mkLuaInline "hl.dsp.exec_cmd(\"pamixer -u && pamixer -i 5\")") ];
          }
          {
            _args = [ "XF86AudioLowerVolume" (mkLuaInline "hl.dsp.exec_cmd(\"pamixer -u && pamixer -d 5\")") ];
          }
          {
            _args = [ "XF86AudioMute" (mkLuaInline "hl.dsp.exec_cmd(\"pamixer -t\")") ];
          }
          {
            _args = [ "XF86MonBrightnessUp" (mkLuaInline "hl.dsp.exec_cmd(\"brightnessctl s +5%\")") ];
          }
          {
            _args = [ "XF86MonBrightnessDown" (mkLuaInline "hl.dsp.exec_cmd(\"brightnessctl s 5%-\")") ];
          }

          # Launchers
          {
            _args = [ "SUPER + d" (mkLuaInline "hl.dsp.exec_cmd(\"rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/themes/launcher\")") ];
          }
          {
            _args = [ "SUPER + SHIFT + q" (mkLuaInline "hl.dsp.exec_cmd(\"rofi-powermenu\")") ];
          }
          {
            _args = [ "SUPER + comma" (mkLuaInline "hl.dsp.exec_cmd(\"cliphist list | rofi -dmenu -display-columns 2 -theme ~/.config/rofi/themes/clipboard | cliphist decode | wl-copy\")") ];
          }
          {
            _args = [ "SUPER + slash" (mkLuaInline "hl.dsp.exec_cmd(\"rofi -show emoji -modi emoji -theme $HOME/.config/rofi/themes/emoji\")") ];
          }
          {
            _args = [ "SUPER + p" (mkLuaInline "hl.dsp.exec_cmd(\"rofi-rbw\")") ];
          }

          # Screenshots
          {
            _args = [ "Print" (mkLuaInline "hl.dsp.exec_cmd(\"grimshot --notify copy\")") ];
          }
          {
            _args = [ "SHIFT + Print" (mkLuaInline "hl.dsp.exec_cmd(\"grimshot --notify save\")") ];
          }
          {
            _args = [ "SUPER + Print" (mkLuaInline "hl.dsp.exec_cmd(\"grimshot --notify copy area\")") ];
          }
          {
            _args = [ "SUPER + SHIFT + Print" (mkLuaInline "hl.dsp.exec_cmd(\"grimshot --notify save area\")") ];
          }

          # Apps
          {
            _args = [ "SUPER + Return" (mkLuaInline "hl.dsp.exec_cmd(\"${terminal}\")") ];
          }
          {
            _args = [ "SUPER + b" (mkLuaInline "hl.dsp.exec_cmd(\"${browser}\")") ];
          }
          {
            _args = [ "SUPER + e" (mkLuaInline "hl.dsp.exec_cmd(\"${editor}\")") ];
          }
          {
            _args = [ "SUPER + f" (mkLuaInline "hl.dsp.exec_cmd(\"${fm}\")") ];
          }

          # Mouse binds
          {
            _args = [
              "SUPER + mouse:272"
              (mkLuaInline "hl.dsp.window.drag()")
              { mouse = true; }
            ];
          }
          {
            _args = [
              "SUPER + mouse:273"
              (mkLuaInline "hl.dsp.window.resize()")
              { mouse = true; }
            ];
          }
        ];
      };
    };

    xdg.portal = {
      enable = true;
      xdgOpenUsePortal = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
      configPackages = [pkgs.inputs.hyprland.hyprland];
      config = {
        common.default = ["*"];
        hyprland.default = ["gtk" "hyprland"];
      };
    };

    mario.modules = {
      apps.dunst.enable = true;
      wayland = {
        locker.enable = true;
        waybar.enable = true;
      };
    };
  };
}
