{
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.wayland;
in {
  imports = [inputs.hyprland.nixosModules.default];

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
        xwayland = {
          enable = true;
          hidpi = true;
        };
      };
    };
  };
}
