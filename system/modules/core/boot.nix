{ options, config, lib, pkgs, ... }: 

with lib;
with lib.my;
let cfg = config.modules.core.boot;
in {
  options.modules.core.boot = {
    splashBoot.enable = mkBoolOpt false;
  };

  config = mkIf cfg.splashBoot.enable {
    boot.consoleLogLevel = 0;
    boot.initrd.verbose = false;
    boot.kernelParams = [ 
      "quiet" 
      "splash"
      "boot.shell_on_fail" 
      "udev.log_priority=3" 
      "loglevel=3" 
      "rd.systemd.show_status=false" 
      "rd.udev.log_level=3" 
    ];

    boot.plymouth = {
      enable = true;
      theme = "bgrt";
    };
  };
}