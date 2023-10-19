{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.wayland;
in {
  options.system.modules.graphical.wayland = {
    enable = mkEnableOption "wayland basic configuration and packages";
  };

  config = mkIf cfg.enable {
    programs = {
      sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = lib.mkDefault [];
      };

      hyprland = {
        enable = true;
        package = pkgs.inputs.hyprland.hyprland;
        xwayland.enable = true;
      };
    };
  };
}
