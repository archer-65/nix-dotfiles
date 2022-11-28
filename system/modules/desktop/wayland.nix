{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.system.modules.desktop.wayland;
in {
  options.system.modules.desktop.wayland = {
    enable = mkEnableOption "wayland basic configuration and packages";
  };

  config = mkIf cfg.enable {
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
  };
}
