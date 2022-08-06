_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.user-modules.editors.android;
in {
  options.user-modules.editors.android = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config =
    mkIf cfg.enable { home.packages = with pkgs; [ android-studio scrcpy ]; };
}
