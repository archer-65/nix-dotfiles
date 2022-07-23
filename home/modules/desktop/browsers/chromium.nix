_:
{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.browsers.chromium;
in {
  options.user-modules.desktop.browsers.chromium = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      package = pkgs.google-chrome-dev;
      commandLineArgs = ["--ozone-platform-hint=auto" ];
    };
  };
}
