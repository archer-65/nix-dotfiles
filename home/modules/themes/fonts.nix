{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes.font;

  mkFontOption = defaultFamily: {
    family = mkOption {
      type = types.str;
      default = defaultFamily;
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
in {
  options.mario.modules.themes.font = {
    regular = mkFontOption "Sans";
    monospace = mkFontOption "Monospace";
  };

  config = {
    fonts.fontconfig.enable = true;
    home.packages = [cfg.regular.package cfg.monospace.package];
  };
}
