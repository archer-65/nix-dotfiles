_:
{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.wayland.sway;
  cfgTheme = config.user-modules.themes;
  configDir = config.dotfiles.configDir;

  import-gsettings = pkgs.writeShellScriptBin "import-gsettings" ''
    config="''${XDG_CONFIG_HOME:-$HOME/.config}/gtk-3.0/settings.ini"
    if [ ! -f "$config" ]; then exit 1; fi

    gnome_schema="org.gnome.desktop.interface"
    gtk_theme="$(grep 'gtk-theme-name' "$config" | cut -d'=' -f2)"
    icon_theme="$(grep 'gtk-icon-theme-name' "$config" | cut -d'=' -f2)"
    cursor_theme="$(grep 'gtk-cursor-theme-name' "$config" | cut -d'=' -f2)"
    font_name="$(grep 'gtk-font-name' "$config" | cut -d'=' -f2)"

    gsettings set "$gnome_schema" gtk-theme "$gtk_theme"
    gsettings set "$gnome_schema" icon-theme "$icon_theme"
    gsettings set "$gnome_schema" cursor-theme "$cursor_theme"
    gsettings set "$gnome_schema" font-name "$font_name"
  '';

in {
  options.user-modules.desktop.wayland.sway = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  imports = [ ./shared.nix ];

  config = mkIf cfg.enable {
    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;

      config = {
        input."type:keyboard" = {
          xkb_layout = "us";
          xkb_variant = "altgr-intl";
          xkb_options = "ctrl:nocaps";
        };

        modifier = "Mod4";
        terminal = "${pkgs.alacritty}/bin/alacritty";

        fonts = {
          names = [ "VictorMono Nerd Font "];
          #size = cfgTheme.font.size;
          size = 15.0;
        };

        gaps.inner = 10;
        gaps.outer = 10;

        focus.followMouse = true;

        window = {
          titlebar = false;
          border = 3;

          commands = [
            { command = "floating enable"; criteria = { class = "Thunar"; }; }
            { command = "floating enable"; criteria = { class = "mpv"; }; }
          ];
        };

        floating = {
          modifier = "Mod4";
          border = 3;
        };

        startup = [
          { command = "if command -v corectrl &> /dev/null ; then corectrl & fi";  }
          { command = "import-gsettings"; always = true; }
          # { command = "exec swhks & ; pkexec swhkd -c $HOME/.config/sway/swhkdrc"; }
        ];

        keybindings =
          let
            mod = config.wayland.windowManager.sway.config.modifier;
            term = config.wayland.windowManager.sway.config.terminal;
          in lib.mkOptionDefault {
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
            "${mod}+Shift+plus" = "gaps inner all plus 5 ; gaps outer all plus 5";
            "${mod}+Shift+minus" = "gaps inner all minus 5 ; gaps outer all minus 5";

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
            "${mod}+Left"  = "workspace prev";

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
          };
      };

      # extraConfig = ''
      #   exec systemctl --user import-environment DISPLAY WAYLAND_DISPLAY SWAYSOCK
      #   exec hash dbus-update-activation-environment 2>/dev/null && \
      #     dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK
      # '';

      extraConfig = ''
        exec dbus-sway-environment
        exec configure-gtk
      '';

      extraSessionCommands = ''
        export MOZ_ENABLE_WAYLAND=1
        export MOZ_DBUS_REMOTE=1
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_QPA_PLATFORMTHEME=qt5ct
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export _JAVA_AWT_WM_NONREPARENTING=1
        export XDG_SESSION_TYPE=wayland
        export XDG_CURRENT_DESKTOP=sway
        export XDG_SESSION_DESKTOP=sway
      '';
    };

   # xdg.configFile."sway/swhkdrc".source = "${configDir}/sway/swhkdrc";

    home.packages = with pkgs; [
      gsettings-desktop-schemas
      import-gsettings
    ];

    user-modules.desktop = {
      services = {
        dunst.enable = true;
      };
    };
  };
}
