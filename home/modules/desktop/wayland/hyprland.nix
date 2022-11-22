{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.wayland;
  cfgTheme = config.mario.modules.themes;
  inherit (config.dotfiles) configDir;
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
          main_mod = SUPER
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
          animation = fade, 1, 7, default
          animation = workspaces, 1, 2, default, fade
        }

        misc {
          no_vfr=false
        }

        dwindle {
          pseudotile = true
          preserve_split = true
        }

        master {
          new_is_master = false
          new_on_top = false
        }

        monitor = desc:AOC U34G2G4R3 0x0000241D, 3440x1440@144, 0x0, 1

        device:Idobao ID80 {
          kb_layout = us
          kb_variant = altgr-intl
          kb_options = ctrl:nocaps
        }

        device:ELECOM TrackBall Mouse HUGE TrackBall {
          scroll_method = on_button_down
          # scroll_button = BTN_TASK
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

        windowrulev2 = float, title:^(Firefox — Sharing Indicator)$
        windowrulev2 = pin, title:^(Firefox — Sharing Indicator)$
        # windowrulev2 = move 100%-20, title:^(Firefox — Sharing Indicator)$

        # idle inhibit while watching videos
        windowrulev2 = idleinhibit focus, class:^(mpv)$
        windowrulev2 = idleinhibit fullscreen, class:^(firefox)$

        # mouse movements
        bindm = SUPER, mouse:272, movewindow
        bindm = SUPER, mouse:273, resizewindow

        exec-once = waybar
        exec = ${pkgs.swaybg}/bin/swaybg -i ~/pics/walls/weebie/wallhaven-j3mmdy.jpg --mode fill;

        exec-once = corectrl
        exec-once = polychromatic-tray-applet

        bind = SUPERCONTROL, r, exec, ${hyprctl} reload

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

        bind = SUPER, left, workspace, -1
        bind = SUPER, right, workspace, +1

        bind = SUPERSHIFT, left, movetoworkspace, -1
        bind = SUPERSHIFT, right, movetoworkspace, +1

        bind = SUPER, w, killactive

        bind = SUPER, h, movefocus, l
        bind = SUPER, j, movefocus, d
        bind = SUPER, k, movefocus, u
        bind = SUPER, l, movefocus, r

        bind = SUPER, t, togglefloating

        bind = SUPERSHIFT, h, movewindow, l
        bind = SUPERSHIFT, j, movewindow, d
        bind = SUPERSHIFT, k,movewindow, u
        bind = SUPERSHIFT, l, movewindow, r

        bind = , XF86AudioRaiseVolume, exec, volume up
        bind = , XF86AudioLowerVolume, exec, volume down
        bind = , XF86AudioMute, exec, volume mute

        bind = SUPER, d, exec, rofi-launcher
        bind = SUPERSHIFT, q, exec, rofi-powermenu
        bind = SUPER, slash, exec, rofi-emoji
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

    home.sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "sway";
      SDL_VIDEODRIVER = "wayland";
      GTK_USE_PORTAL = "1";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      MOZ_ENABLE_WAYLAND = "1";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      NIXOS_OZONE_WL = "1";
    };

    mario.modules.desktop = {
      services = {
        dunst.enable = true;
        locker-wayland.enable = true;
        waybar.enable = true;
      };
    };
  };
}
