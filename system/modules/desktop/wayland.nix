_:
{ lib, config, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.wayland;
in {
  options.modules.desktop.wayland = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ sway wayland glib ];

    programs.sway = {
      enable = true;

      wrapperFeatures = {
        base = true;
        gtk = true;
      };
    };
  };
}
