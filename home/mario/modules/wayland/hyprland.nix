{
  config,
  lib,
  pkgs,
  inputs,
  outputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.wayland;
  inherit (config.colorScheme) colors;
in {
  config = mkIf (cfg.enable && (elem "hyprland" cfg.wm)) {
    wayland.windowManager.hyprland = {
      enable = true;
      package = pkgs.inputs.hyprland.hyprland;

      xwayland.enable = true;
      systemd.enable = true;

      settings =  let
        hyprctl = "${pkgs.hyprland}/bin/hyprctl";
        terminal = "${pkgs.alacritty}/bin/alacritty";
        browser = "${pkgs.firefox}/bin/firefox";
        editor = "emacsclient -c";
        fm = "thunar";
      in {
        general = {
          gaps_in = 6;
          gaps_out = 6;
          border_size = 3.5;
          layout = "master";
          "col.active_border" = "0xff${colors.base0B}";
          "col.inactive_border" = "0xff${colors.base02}";
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
          animation = [
            "windows, 1, 4, default, slide"
            "border, 1, 5, default"
            "fade, 1, 0.1, default"
            "workspaces, 1, 2, default, fade"
          ];
        };

        master = {
          new_is_master = false;
          new_on_top = false;
        };

        dwindle = {
          pseudotile = true;
          preserve_split = true;
        };

        gestures = {
          workspace_swipe = true;
          workspace_swipe_fingers = 3;
        };

        misc = {
          vfr = false;
          enable_swallow = true;
          swallow_regex = "^(Alacritty)$";
        };

        monitor = [
          # Home
          "desc:AOC U34G2G4R3 0x0000241D, 3440x1440@144, 0x0, 1'"
          # Work
          "desc:BOE 0x083C, 1920x1080@60, 0x0, 1"
          "desc:AU Optronics 0xED8F, 1920x1080@60, 0x0, 1"
        ];

        input = {
          kb_layout = "us";
          kb_variant = "altgr-intl";
          kb_options = "ctrl:nocaps";
          accel_profile = "flat";
          follow_mouse = 1;
        };

        "device:elecom-trackball-mouse-huge-trackball-1" = {
          scroll_method = "on_button_down";
          scroll_button = 279;
          accel_profile = "adaptive";
        };

        windowrulev2 = [
          # Needed
          "float, class:^(thunar)$"
          "float, class:^(Rofi)$"
          "noborder, class:^(Rofi)$"
          "float, class:^(ipv)$"
          "float, class:^(mpv)$"
          "float, class:^(pavucontrol)$"
          "float, title:^(Polychromatic)$"

          # Popups
          "float, class:^(GtkFileChooserDialog)$"
          "float, class:^(pop-up)$"
          "float, class:^(Organizer)$"
          "float, class:^(task_dialog)$"

          # Browser indicators
          "float, class:^(firefox)$, title:^(Picture-in-Picture)"
          "pin, class:^(firefox)$, title:^(Picture-in-Picture)"

          "workspace special:trash silent, title:^(Firefox — Sharing Indicator)$"
          # "float, title:^(Firefox — Sharing Indicator)$"
          # "pin, title:^(Firefox — Sharing Indicator)$"
          # "move 100%-20, title:^(Firefox — Sharing Indicator)$"

          # idle inhibit while watching videos
          "idleinhibit focus, class:^(mpv)$"
          "idleinhibit fullscreen, class:^(firefox)$"
        ];

        exec = [
          "${pkgs.swaybg}/bin/swaybg -i ${outputs.wallpapers.digital-flowers.src} --mode fill"
        ];

        exec-once = [
          "waybar"

          "wl-paste -t text --watch clipman store --no-persist"
          "wl-paste -p -t text --watch clipman store -P --histpath=\"~/.local/share/clipman-primary.json\""

          "corectrl"
          "polychromatic-tray-applet"
        ];

        bindm = [
          # Resizing with
          "SUPER, mouse:272, movewindow"
          "SUPER, mouse:273, resizewindow"
        ];

         bind = [
           "SUPER, g, togglegroup"
           "SUPER, apostrophe, changegroupactive, f"
           "SUPERSHIFT, apostrophe, changegroupactive, b"

           "SUPER, m, layoutmsg, focusmaster"
           "SUPERSHIFT, m, layoutmsg, swapwithmaster"
           "SUPER, down, layoutmsg, cyclenext"
           "SUPER, up, layoutmsg, cycleprev"
           "SUPERSHIFT, down, layoutmsg,swapnext"
           "SUPERSHIFT, up, layoutmsg, swapprev"

           # Toggle bar
           "SUPERCTRL, F1, exec, ${pkgs.procps}/bin/pkill -USR1 waybar"
           "SUPERSHIFT, F1, exec, ${pkgs.procps}/bin/pkill waybar && waybar"

           "SUPERCTRL, d, exec, ${hyprctl} keyword general:layout dwindle"
           "SUPERCTRL, m, exec, ${hyprctl} keyword general:layout master"

           "SUPER, 1, workspace, 01"
           "SUPER, 2, workspace, 02"
           "SUPER, 3, workspace, 03"
           "SUPER, 4, workspace, 04"
           "SUPER, 5, workspace, 05"
           "SUPER, 6, workspace, 06"
           "SUPER, 7, workspace, 07"
           "SUPER, 8, workspace, 08"
           "SUPER, 9, workspace, 09"

           "SUPERSHIFT, 1, movetoworkspace, 01"
           "SUPERSHIFT, 2, movetoworkspace, 02"
           "SUPERSHIFT, 3, movetoworkspace, 03"
           "SUPERSHIFT, 4, movetoworkspace, 04"
           "SUPERSHIFT, 5, movetoworkspace, 05"
           "SUPERSHIFT, 6, movetoworkspace, 06"
           "SUPERSHIFT, 7, movetoworkspace, 07"
           "SUPERSHIFT, 8, movetoworkspace, 08"
           "SUPERSHIFT, 9, movetoworkspace, 09"

           "SUPER, h, movefocus, l"
           "SUPER, j, movefocus, d"
           "SUPER, k, movefocus, u"
           "SUPER, l, movefocus, r"

           "SUPERSHIFT, h, movewindow, l"
           "SUPERSHIFT, j, movewindow, d"
           "SUPERSHIFT, k,movewindow, u"
           "SUPERSHIFT, l, movewindow, r"

           "SUPER, left, workspace, -1"
           "SUPER, right, workspace, +1"

           "SUPERSHIFT, left, movetoworkspace, -1"
           "SUPERSHIFT, right, movetoworkspace, +1"


           "SUPER, bracketleft, focusmonitor, l"
           "SUPER, bracketright, focusmonitor, r"

           "SUPERSHIFT, backslash, movetoworkspace, special"
           "SUPER, backslash, togglespecialworkspace"



           "SUPERCTRL, h, resizeactive, -50 0"
           "SUPERCTRL, j, resizeactive, 0 -50"
           "SUPERCTRL, k, resizeactive, 0 50"
           "SUPERCTRL, l, resizeactive, 50 0"

           "SUPER, w, killactive"

           "SUPER, t, togglefloating"

           ",F11, fullscreen, 0"
           "SUPER, F11, fullscreen, 1"

           ", XF86AudioRaiseVolume, exec, pamixer -u && pamixer -i 5"
           ", XF86AudioLowerVolume, exec, pamixer -u && pamixer -d 5"
           ", XF86AudioMute, exec, pamixer -t"

           ", XF86MonBrightnessUp, exec, brightnessctl s +5%"
           ", XF86MonBrightnessDown, exec, brightnessctl s 5%-"

           "SUPER, d, exec, rofi -no-lazy-grab -show drun -modi run,drun -theme $HOME/.config/rofi/themes/launcher"
           "SUPERSHIFT, q, exec, rofi-powermenu"
           "SUPER, comma, exec, clipman pick -t rofi -T'-theme ~/.config/rofi/themes/clipboard'"
           "SUPER, slash, exec, rofi -show emoji -modi emoji -theme $HOME/.config/rofi/themes/emoji"
           "SUPER, p, exec, rofi-rbw"

           ", Print, exec, grimshot --notify copy"
           "SHIFT, Print, exec, grimshot --notify save"
           "SUPER, Print, exec, grimshot --notify copy area"
           "SUPERSHIFT, Print, exec, grimshot --notify save area"

           "SUPER, Return, exec, ${terminal}"
           "SUPER, b, exec, ${browser}"
           "SUPER, e, exec, ${editor}"
           "SUPER, f, exec, ${fm}"
         ];
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
