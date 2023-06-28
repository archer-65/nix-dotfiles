{
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.displaylink;
in {
  options.system.modules.graphical.displaylink = {
    enable = mkEnableOption "enable displaylink support";
  };

  config = mkIf cfg.enable {
    services.xserver.videoDrivers = ["displaylink" "modesetting"];

    # NOTE: Just to try wlroots + displaylink
    # environment.etc."modprobe.d/evdi.conf".text = ''
    #   softdep evdi pre: i915 drm_display_helper
    #   options evdi initial_device_count=2 initial_loglevel=3
    # '';
  };
}
