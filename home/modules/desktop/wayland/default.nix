{ config, options, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.desktop.wayland;
in {
  options.user-modules.desktop.wayland = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };

    wm = mkOption {
      default = null;
      type = types.nullOr (types.enum [ "sway" "hyprland" ]);
      example = "sway";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      wl-clipboard
      wtype
      # azote
      # swhkd
      grim
      slurp
      sway-contrib.grimshot
    ];
  };
}
