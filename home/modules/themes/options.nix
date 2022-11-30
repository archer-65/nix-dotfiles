{
  pkgs,
  options,
  config,
  lib,
  nix-colors,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes;
  inherit (nix-colors.lib-contrib {inherit pkgs;}) gtkThemeFromScheme;
  inherit (config.mario.modules) xorg;
in {
  options.mario.modules.themes = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        Name of the theme to enable.
      '';
    };

    darkTheme = mkOption {
      type = types.bool;
      default = false;
      description = ''
        If available, set the variant of chosen theme to light/dark one.
      '';
    };

    # GTK & Co.
    ui.font = {
      name = mkOption {
        type = str;
        default = "Sans";
      };

      size = mkOption {
        type = int;
        default = 12;
      };
    };

    # Terminal font
    term.font = {
      name = mkOption {
        type = str;
        default = "Monospace";
      };

      size = mkOption {
        type = int;
        default = 14;
      };
    };

    # Alternative (notifications, etc.)
    ui-alt.font = {
      name = mkOption {
        type = str;
        default = "Monospace";
      };

      size = mkOption {
        type = int;
        default = 12;
      };
    };

    cursor.size = mkOption {
      type = int;
      default = 16;
    };

    # Bar settings
    bar = {
      font = {
        name = mkOption {
          type = str;
          default = "Monospace";
        };

        size = mkOption {
          type = int;
          default = 12;
        };
      };

      battery = mkOption {
        type = nullOr str;
        default = null;
      };
    };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    {
      home.sessionVariables = {QT_QPA_PLATFORMTHEME = "qt5ct";};
      home.packages = with pkgs.libsForQt5; [
        qtstyleplugin-kvantum
        breeze-qt5
        qt5ct
      ];

      gtk = {
        enable = true;

        theme = {
          name = "${config.colorscheme.slug}";
          package = gtkThemeFromScheme {scheme = config.colorscheme;};
        };

        font = {
          inherit (cfg.ui.font) name;
          inherit (cfg.ui.font) size;
        };
      };

      gtk.gtk3.extraConfig = let
        is-dark =
          if cfg.darkTheme
          then 1
          else 0;
      in {
        gtk-application-prefer-dark-theme = is-dark;
      };

      home.pointerCursor = {
        inherit (config.gtk.cursorTheme) name size package;
        gtk.enable = true;
      };
    }

    (mkIf xorg.enable {
      home.pointerCursor.x11.enable = true;
      xresources.properties = {"Xcursor.theme" = config.gtk.cursorTheme.name;};
    })
  ]);
}
