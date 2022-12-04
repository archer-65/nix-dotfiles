{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.term;
in {
  options.mario.modules.term = {
    font = {
      family = mkOption {
        type = types.str;
        default = "Monospace";
      };

      package = lib.mkOption {
        type = types.package;
        default = null;
      };

      size = mkOption {
        type = types.int;
        default = 12;
      };
    };
  };

  config = {
    fonts.fontconfig.enable = true;
    home.packages = [cfg.font.package];
  };
}
