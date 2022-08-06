_:
{ config, options, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.desktop.xorg;
in {
  options.user-modules.desktop.xorg = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ nitrogen xclip xdotool ];

    services = { flameshot.enable = true; };

    user-modules.desktop = {
      apps = { greenclip.enable = true; };
      services = { locker.enable = true; };
    };
  };
}
