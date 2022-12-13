{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes.bar;
in {
  options.mario.modules.themes.bar = {
    font = {
      family = mkOption {
        type = types.str;
        default = "Monospace";
      };

      package = mkOption {
        type = types.package;
        default = null;
      };

      size = mkOption {
        type = types.int;
        default = 12;
      };
    };

    battery = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    temperature = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = {
    fonts.fontconfig.enable = true;
    home.packages = [cfg.font.package];
  };
}
