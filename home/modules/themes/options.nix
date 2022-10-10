_:
{ pkgs, options, config, lib, ... }:

with lib;
let cfg = config.user-modules.themes;
in {
  options.user-modules.themes = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v:
        let theme = builtins.getEnv "THEME";
        in if theme != "" then theme else v;
      description = ''
        Name of the theme to enable. Can be overridden by the THEME environment
        variable. Themes can also be hot-swapped with 'hey theme $THEME'.
      '';
    };

    darkTheme = mkOption {
      type = types.bool;
      default = false;
      description = ''
        If available, set the variant of chosen theme to light/dark one.
      '';
    };

    font = {
      # GTK & Co.
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
    font.term = {
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
    font.alt = {
      name = mkOption {
        type = str;
        default = "Monospace";
      };

      size = mkOption {
        type = int;
        default = 12;
      };
    };
  };

  config = mkIf (cfg.active != null) {
    # Well, this file is called "options.nix", but I put every global theming utility here.
    home.packages = with pkgs.libsForQt5; [
      qtstyleplugin-kvantum
      breeze-qt5
      qt5ct
    ];
    home.sessionVariables = { QT_QPA_PLATFORMTHEME = "qt5ct"; };
  };
}
