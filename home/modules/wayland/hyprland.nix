{
  config,
  lib,
  pkgs,
  wallpapers,
  ...
}:
with lib; let
  cfg = config.mario.modules.wayland;
  inherit (config.colorScheme) colors;
in {
  config = mkIf (cfg.enable && (elem "hyprland" cfg.wm)) {
    wayland.windowManager.hyprland = {
      enable = true;
      xwayland = {
        enable = true;
        hidpi = true;
      };
      systemdIntegration = true;

      extraConfig = let
        hyprctl = "${pkgs.hyprland}/bin/hyprctl";

        terminal = "${pkgs.alacritty}/bin/alacritty";
        browser = "${pkgs.firefox}/bin/firefox";
        editor = "emacsclient -c";
        fm = "thunar";
      in ''
        general {
          gaps_in = 6
          gaps_out = 6
          border_size = 3.5
          col.active_border=0xff${colors.base0B}
          col.inactive_border=0xff${colors.base02}
          layout = master
        }

        decoration {
          active_opacity = 1.0
          inactive_opacity = 0.98
          fullscreen_opacity = 1.0
          rounding = 5
          blur = false
        }

        animations {
          enabled = true
          animation = windows, 1, 4, default, slide
          animation = border, 1, 5, default
          animation = fade, 1, 0.1, default
          animation = workspaces, 1, 2, default, fade
        }

        gestures {
          workspace_swipe = true
          workspace_swipe_fingers = 3
        }

        misc {
          no_vfr = true
          enable_swallow = true
          swallow_regex = ^(Alacritty)$
        }


        # Dwindle layout section
        dwindle {
          pseudotile = true
          preserve_split = true
        }

        bind = SUPER, g, togglegroup
        bind = SUPER, apostrophe, changegroupactive, f
        bind = SUPERSHIFT, apostrophe, changegroupactive, b

        # Master layout section
        master {
          new_is_master = false
          new_on_top = false
        }

        bind = SUPER, m, layoutmsg, focusmaster
        bind = SUPERSHIFT, m, layoutmsg, swapwithmaster
        bind = SUPER, down, layoutmsg, cyclenext
        bind = SUPER, up, layoutmsg, cycleprev
        bind = SUPERSHIFT, down, layoutmsg,swapnext
        bind = SUPERSHIFT, up, layoutmsg, swapprev

        monitor = desc:AOC U34G2G4R3 0x0000241D, 3440x1440@144, 0x0, 1
        monitor = desc:BOE 0x083C, 1920x1080@60, 0x0, 1

        input {
          kb_layout = us
          kb_variant = altgr-intl
          kb_options = ctrl:nocaps
        }

        input {
          accel_profile = flat
          follow_mouse = 1
        }

        device:elecom-trackball-mouse-huge-trackball-1 {
          scroll_method = on_button_down
          scroll_button = 279
          accel_profile = adaptive
        }

        # Needed
        windowrulev2 = float, class:^(thunar)$
        windowrulev2 = float, class:^(Rofi)$
        windowrulev2 = noborder, class:^(Rofi)$
        windowrulev2 = float, class:^(ipv)$
        windowrulev2 = float, class:^(mpv)$
        windowrulev2 = float, class:^(pavucontrol)$
        windowrulev2 = float, title:^(Polychromatic)$

        # Popups
        windowrulev2 = float, class:^(GtkFileChooserDialog)$
        windowrulev2 = float, class:^(pop-up)$
        windowrulev2 = float, class:^(Organizer)$
        windowrulev2 = float, class:^(task_dialog)$

        # Browser indicators
        windowrulev2 = float, class:^(firefox)$, title:^(Picture-in-Picture)
        windowrulev2 = pin, class:^(firefox)$, title:^(Picture-in-Picture)

        windowrulev2 = workspace special:trash silent, title:^(Firefox — Sharing Indicator)$
        # windowrulev2 = float, title:^(Firefox — Sharing Indicator)$
        # windowrulev2 = pin, title:^(Firefox — Sharing Indicator)$
        # windowrulev2 = move 100%-20, title:^(Firefox — Sharing Indicator)$

        # idle inhibit while watching videos
        windowrulev2 = idleinhibit focus, class:^(mpv)$
        windowrulev2 = idleinhibit fullscreen, class:^(firefox)$

        exec-once = waybar
        exec = ${pkgs.swaybg}/bin/swaybg -i ${wallpapers.moonlight-car.src} --mode fill

        exec-once = wl-paste -t text --watch clipman store --no-persist
        exec-once = wl-paste -p -t text --watch clipman store -P --histpath=\"~/.local/share/clipman-primary.json\"

        exec-once = corectrl
        exec-once = polychromatic-tray-applet

        # Toggle bar
        bind = SUPERCTRL, F1, exec, ${pkgs.procps}/bin/pkill -USR1 waybar
        bind = SUPERSHIFT, F1, exec, ${pkgs.procps}/bin/pkill waybar && waybar

        bind = SUPERCTRL, d, exec, ${hyprctl} keyword general:layout dwindle
        bind = SUPERCTRL, m, exec, ${hyprctl} keyword general:layout master

        bind = SUPER, 1, workspace, 01
        bind = SUPER, 2, workspace, 02
        bind = SUPER, 3, workspace, 03
        bind = SUPER, 4, workspace, 04
        bind = SUPER, 5, workspace, 05
        bind = SUPER, 6, workspace, 06
        bind = SUPER, 7, workspace, 07
        bind = SUPER, 8, workspace, 08
        bind = SUPER, 9, workspace, 09

        bind = SUPERSHIFT, 1, movetoworkspace, 01
        bind = SUPERSHIFT, 2, movetoworkspace, 02
        bind = SUPERSHIFT, 3, movetoworkspace, 03
        bind = SUPERSHIFT, 4, movetoworkspace, 04
        bind = SUPERSHIFT, 5, movetoworkspace, 05
        bind = SUPERSHIFT, 6, movetoworkspace, 06
        bind = SUPERSHIFT, 7, movetoworkspace, 07
        bind = SUPERSHIFT, 8, movetoworkspace, 08
        bind = SUPERSHIFT, 9, movetoworkspace, 09

        bind = SUPER, h, movefocus, l
        bind = SUPER, j, movefocus, d
        bind = SUPER, k, movefocus, u
        bind = SUPER, l, movefocus, r

        bind = SUPERSHIFT, h, movewindow, l
        bind = SUPERSHIFT, j, movewindow, d
        bind = SUPERSHIFT, k,movewindow, u
        bind = SUPERSHIFT, l, movewindow, r

        bind = SUPER, left, workspace, -1
        bind = SUPER, right, workspace, +1

        bind = SUPERSHIFT, left, movetoworkspace, -1
        bind = SUPERSHIFT, right, movetoworkspace, +1


        bind = SUPER, bracketleft, focusmonitor, l
        bind = SUPER, bracketright, focusmonitor, r

        bind = SUPERSHIFT, backslash, movetoworkspace, special
        bind = SUPER, backslash, togglespecialworkspace

        # Resizing
        bindm = SUPER, mouse:272, movewindow
        bindm = SUPER, mouse:273, resizewindow

        bind = SUPERCTRL, h, resizeactive, -50 0
        bind = SUPERCTRL, j, resizeactive, 0 -50
        bind = SUPERCTRL, k, resizeactive, 0 50
        bind = SUPERCTRL, l, resizeactive, 50 0

        bind = SUPER, w, killactive

        bind = SUPER, t, togglefloating

        bind = ,F11, fullscreen, 0
        bind = SUPER, F11, fullscreen, 1

        bind = , XF86AudioRaiseVolume, exec, pamixer -u && pamixer -i 5
        bind = , XF86AudioLowerVolume, exec, pamixer -u && pamixer -d 5
        bind = , XF86AudioMute, exec, pamixer -t

        bind = SUPER, d, exec, rofi -no-lazy-grab -show drun -modi run,drun -theme $HOME/.config/rofi/themes/launcher
        bind = SUPERSHIFT, q, exec, rofi-powermenu
        bind = SUPER, comma, exec, clipman pick -t rofi -T'-theme ~/.config/rofi/themes/clipboard'
        bind = SUPER, slash, exec, rofi -show emoji -modi emoji -theme $HOME/.config/rofi/themes/emoji
        bind = SUPER, p, exec, rofi-rbw

        bind = , Print, exec, grimshot --notify copy
        bind = SHIFT, Print, exec, grimshot --notify save
        bind = SUPER, Print, exec, grimshot --notify copy area
        bind = SUPERSHIFT, Print, exec, grimshot --notify save area

        bind = SUPER, Return, exec, ${terminal}
        bind = SUPER, b, exec, ${browser}
        bind = SUPER, e, exec, ${editor}
        bind = SUPER, f, exec, ${fm}
      '';
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
