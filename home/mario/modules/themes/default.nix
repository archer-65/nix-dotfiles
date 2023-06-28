{
  pkgs,
  options,
  config,
  lib,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes;
  inherit (inputs.nix-colors.lib-contrib {inherit pkgs;}) gtkThemeFromScheme;
  inherit (config.mario.modules) xorg;
in {
  imports = [ ./onedark.nix ./modus.nix ];

  options.mario.modules.themes = with types; {
    active = mkOption {
      type = nullOr (enum [ "onedark" "modus" ]);
      default = null;
      description = ''
        Name of the theme to enable.
      '';
    };

    darkTheme = mkOption {
      type = bool;
      default = false;
      description = ''
        If available, set the variant of chosen theme to light/dark one.
      '';
    };

    cursor.size = mkOption {
      type = int;
      default = 16;
    };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    # GTK
    {
      home.packages = [pkgs.theme-toggle];

      gtk = {
        enable = true;

        theme = {
          name = "${config.colorscheme.slug}";
          package = gtkThemeFromScheme {scheme = config.colorscheme;};
        };

        font = {
          name = cfg.font.regular.family;
          inherit (cfg.font.regular) size;
        };

        gtk3.extraConfig = let
          is-dark =
            if cfg.darkTheme
            then 1
            else 0;
        in {
          gtk-application-prefer-dark-theme = is-dark;
        };
      };

      home.pointerCursor = {
        inherit (config.gtk.cursorTheme) name size package;
        gtk.enable = true;
      };
    }

    # QT
    {
      home.sessionVariables = {QT_QPA_PLATFORMTHEME = "qt5ct";};
      home.packages = with pkgs.libsForQt5; [
        qtstyleplugin-kvantum
        breeze-qt5
        qt5ct
      ];
    }

    # Xorg
    (mkIf xorg.enable {
      home.pointerCursor.x11.enable = true;
      xresources.properties = {"Xcursor.theme" = config.gtk.cursorTheme.name;};
    })
  ]);
}
