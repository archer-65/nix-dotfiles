{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.boot;
in {
  options.system.modules.core.boot = {
    quietboot.enable = mkEnableOption "quietboot";
  };

  config = mkIf cfg.quietboot.enable {
    boot.plymouth = {
      enable = true;
      theme = "bgrt";
    };

    boot = {
      initrd.verbose = false;
      boot.consoleLogLevel = 0;
    };

    boot.kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "loglevel=3"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "udev.log_priority=3"
      "vt.global_cursor_default=0"
    ];

    console = {
      useXkbConfig = true;
      earlySetup = false;
    };
  };
}
