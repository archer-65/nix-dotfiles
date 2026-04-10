{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes;
in {
  imports = [
    inputs.stylix.homeModules.stylix
    ./modus.nix
  ];

  options.mario.modules.themes = with types; {
    active = mkOption {
      type = nullOr (enum ["modus"]);
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
    # Globally enabled, for every system if a theme is selected
    {
      stylix.enable = true;
      stylix.autoEnable = false;
    }

    # Only on Linux because... GTK and QT
    (mkIf pkgs.stdenv.isLinux {
      stylix.targets.gtk.enable = true;
      stylix.targets.gtk.fonts.enable = false;

      # NOTE: I have older `stateVersion` values for home-manager, this is the new behavior
      gtk.gtk4.theme = null;
      gtk.font = {
        name = cfg.font.regular.family;
        inherit (cfg.font.regular) size;
      };

      stylix.targets.qt.enable = true;
    })
  ]);
}
