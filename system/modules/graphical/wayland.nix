{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.wayland;
in {
  options.system.modules.graphical.wayland = {
    enable = mkEnableOption "wayland basic configuration and packages";
  };

  config = mkIf cfg.enable {
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
  };
}
