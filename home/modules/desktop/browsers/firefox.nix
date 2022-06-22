_:
{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.browsers.firefox;
  cfgWayland = config.user-modules.desktop.wayland;
in {
  options.user-modules.desktop.browsers.firefox = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package =
        if cfgWayland.enable then pkgs.firefox-wayland else pkgs.firefox;
    };
  };
}
